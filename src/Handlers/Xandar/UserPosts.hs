{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- ^
-- Handlers for the user posts endpoints
module Handlers.Xandar.UserPosts where

import Api.Xandar
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor
import Data.List (intercalate)
import Data.Text (Text)
import Database.Bloodhound (SearchResult(..), EsError, Search)
import Handlers.Xandar.Common
       (throwApiError, mkPagination, splitLabels, mkGetMultipleResult,
        mkCreateMultipleResult, mkApiResponse, dbName, dbPipe,
        getCreateLink, mkLink, mkSingleResult)
import qualified Handlers.Xandar.Common as C
import Parsers.Filter (parse)
import Persistence.Common
import Persistence.ElasticSearch
import Persistence.Xandar.Common (userPostDefinition, runEs)
import Persistence.Xandar.UserPosts (insertUserPosts)
import Servant
import Types.Common

-- ^
-- Elastic-search mapping name
mappingName :: Text
mappingName = recordCollection userPostDefinition

-- ^
-- Elastic-search index
esIndex :: ApiConfig -> Text
esIndex = confGetEsIndex appName

-- ^
-- Get a single record by id
getSingle :: Maybe Text -> Text -> Api (Headers '[Header "ETag" String] Record)
getSingle etag uid = mkApiResponse respond (runEs' $ getByIds [uid])
  where
    respond = maybe (throwError err404) (mkSingleResult etag) . extractRecord

-- ^
-- Get multiple records
getMultiple :: ApiGetMultipleLink -> ServerT GetMultiple Api
getMultiple getLink include query sortFields page perPage =
  mkApiResponse (return . respond) getRecords
  where
    getRecords = getMultiple' include query sortFields page perPage
    mkUrl page' = getLink include query sortFields (Just page') perPage
    respond = uncurry (mkGetMultipleResult mkUrl)

-- ^
-- Helper for `getMultiple`
getMultiple'
  :: (MonadReader ApiConfig m, MonadIO m)
  => [Text]
  -> Maybe Text
  -> [Text]
  -> Maybe Int
  -> Maybe Int
  -> ExceptT ApiError m ([Record], Pagination)
getMultiple' include query sortFields page perPage = do
  count <- runEs' . countDocuments =<< getCountSearch
  let pagination = mkPagination page perPage count
  let start = paginationStart pagination
  let limit = paginationLimit pagination
  search <- getSearch start limit
  results <- runEs' . searchDocuments $ search
  return (extractRecords include' results, pagination)
  where
    getCountSearch = ExceptT . return $ prepareSearch include' query sort' 0 0
    getSearch start = ExceptT . return . prepareSearch [] query sort' start
    include' = splitLabels include
    sort' = splitLabels sortFields

-- ^
-- Delete a single record
deleteSingle :: Text -> Api NoContent
deleteSingle uid = verify >> mkApiResponse (const $ return NoContent) delete
  where
    verify = getSingle mempty uid
    delete = runEs' $ deleteDocument uid

-- ^
-- Create one or more records
createSingleOrMultiple
  :: (Text -> String)
  -> ApiData Record
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingleOrMultiple getLink (Single r) = createSingle getLink r
createSingleOrMultiple getLink (Multiple rs) = createMultiple getLink rs

-- ^
-- Create a single record
createSingle
  :: (Text -> String)
  -> Record
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingle getLink input = mkApiResponse respond (createSingle' True input)
  where
    success r = addHeader (getCreateLink getLink r) $ noHeader (Single $ Succ r)
    respond = apiItem throwApiError (return . success)

-- ^
-- Create multiple records
createMultiple
  :: (Text -> String)
  -> [Record]
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createMultiple getLink inputs =
  mkApiResponse (mkCreateMultipleResult getLink) (createMultiple' True inputs)

-- ^
-- Update (replace) a single record
replaceSingle :: Text -> Record -> Api Record
replaceSingle = updateSingle True

-- ^
-- Update (modify) a single record
modifySingle :: Text -> Record -> Api Record
modifySingle = updateSingle False

-- ^
-- Update (replace) multiple records
replaceMultiple :: [Record] -> Api [ApiResult]
replaceMultiple = updateMultiple True

-- ^
-- Update (modify) multiple records
modifyMultiple :: [Record] -> Api [ApiResult]
modifyMultiple = updateMultiple False

-- ^
-- Handle an options request for a single record endpoint
optionsSingle :: Text
              -> Api (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
optionsSingle = C.optionsSingle

-- ^
-- Handle an options request for a multiple record endpoint
optionsMultiple :: Api (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
optionsMultiple = C.optionsMultiple

-- ^
-- Helper for create or update single methods
createSingle'
  :: (MonadReader ApiConfig m, MonadIO m)
  => Bool -> Record -> ExceptT ApiError m ApiResult
createSingle' replace input = head <$> createMultiple' replace [input]

-- ^
-- Helper for create or update multiple methods
createMultiple'
  :: (MonadReader ApiConfig m, MonadIO m)
  => Bool -> [Record] -> ExceptT ApiError m [ApiResult]
createMultiple' replace inputs = do
  conf <- ask
  pipe <- dbPipe conf
  ExceptT $
    liftIO $
    runExceptT $
    insertUserPosts
      replace
      inputs
      (dbName conf, pipe)
      (esServer conf, esIndex conf)

-- ^
-- Update multiple records
updateMultiple :: Bool -> [Record] -> Api [ApiResult]
updateMultiple replace inputs = mkApiResponse return (createMultiple' replace inputs)

-- ^
-- Update a single record
updateSingle :: Bool -> Text -> Record -> Api Record
updateSingle replace uid input = mkApiResponse respond run
  where
    respond = apiItem throwApiError return
    run = updateSingle' replace uid input

-- ^
-- Helper for update single methods
updateSingle'
  :: (MonadReader ApiConfig m, MonadIO m)
  => Bool -> Text -> Record -> ExceptT ApiError m ApiResult
updateSingle' replace uid input = do
  result <- runEs' (getByIds [uid])
  existing <- ExceptT $ return (extract result)
  createSingle' replace (merge existing input)
  where
    merge old new = mergeRecords new (includeFields include old)
    include = ["postId", "subscriptionId"]
    extract r = maybe (Left mkApiError404) Right (extractRecord r)

-- ^
-- Run an elastic-search action
runEs'
  :: (MonadReader ApiConfig m, MonadIO m)
  => (Text -> Text -> Text -> IO (Either EsError d)) -> ExceptT ApiError m d
runEs' action = do
  conf <- ask
  runEs action (esServer conf) (esIndex conf) mappingName

-- ^
-- Make a 'Search' object from query string parameters
prepareSearch
  :: [Text]
  -> Maybe Text
  -> [Text]
  -> RecordStart
  -> ResultLimit
  -> Either ApiError Search
prepareSearch include query sortFields start limit = expr >>= toSearch
  where
    expr :: Either ApiError (Maybe FilterExpr)
    expr = sequence $ first toErr . parse <$> query
    toSearch e = first esToApiError $ mkSearch e sortFields include start limit
    toErr msg = mkApiError400 ("Invalid query: " ++ msg)