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
import Control.Monad.Trans.Control
import Data.Bifunctor
import Data.Text (Text)
import Database.Bloodhound (Search, EsError)
import Handlers.Xandar.Common
       (throwApiError, mkPagination, splitLabels, mkGetMultipleResult,
        mkCreateMultipleResult, mkApiResponse, getCreateLink,
        mkGetSingleResult, mkCreateSingleResult)
import qualified Handlers.Xandar.Common as C
import Parsers.Filter (parse)
import Persistence.Common
import Persistence.ElasticSearch
import Persistence.Facade (runEs, dbPipe)
import Persistence.Xandar.Common (userPostDefinition)
import Persistence.Xandar.UserPosts
       (insertUserPosts, esInsert, esInsertMulti, esUpdate, esUpdateMulti)
import Servant
import Types.Common

-- ^
-- Elastic-search mapping name
mappingName :: Text
mappingName = recordCollection userPostDefinition

-- ^
-- Get a single record by id
getSingle :: Maybe Text -> Text -> Api (Headers '[Header "ETag" String] Record)
getSingle etag uid = mkApiResponse respond (runEs $ getByIds [uid] mappingName)
  where
    respond = maybe (throwError err404) (mkGetSingleResult etag) . extractRecord

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
  :: (MonadReader ApiConfig m, MonadIO m, MonadBaseControl IO m)
  => [Text]
  -> Maybe Text
  -> [Text]
  -> Maybe Int
  -> Maybe Int
  -> ExceptT ApiError m ([Record], Pagination)
getMultiple' include query sortFields page perPage = do
  cnt <- runEs . count' =<< getCountSearch
  let pagination = mkPagination page perPage cnt
  let start = paginationStart pagination
  let limit = paginationLimit pagination
  search <- getSearch start limit
  results <- runEs . (`searchDocuments` mappingName) $ search
  return (extractRecords include' results, pagination)
  where
    getCountSearch = ExceptT . return $ prepareSearch include' query sort' 0 0
    getSearch start = ExceptT . return . prepareSearch [] query sort' start
    include' = splitLabels include
    sort' = splitLabels sortFields
    count' s = countDocuments s mappingName

-- ^
-- Delete a single record
deleteSingle :: Text -> Api NoContent
deleteSingle uid = verify >> mkApiResponse (const $ return NoContent) delete
  where
    verify = getSingle mempty uid
    delete = runEs $ deleteDocument uid mappingName

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
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingle getLink input = insert (mkCreateSingleResult getLink)
  where
    insert f = do
      result <- runExceptT $ esInsert input
      either throwApiError f result

-- ^
-- Create multiple records
createMultiple
  :: (Text -> String)
  -> [Record]
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createMultiple getLink input = insert >>= mkCreateMultipleResult getLink
  where
    insert = do
      records <- runApiItems2T $ esInsertMulti input
      return $ itemsToApiResults records

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
replaceMultiple :: [Record] -> Api ApiResults
replaceMultiple = updateMultiple True

-- ^
-- Update (modify) multiple records
modifyMultiple :: [Record] -> Api ApiResults
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
createSingle' replace input =
  head . apiItems <$> createMultiple' replace [input]

-- ^
-- Helper for create or update multiple methods
createMultiple'
  :: (MonadReader ApiConfig m, MonadIO m)
  => Bool -> [Record] -> ExceptT ApiError m ApiResults
createMultiple' replace input = do
  conf <- ask
  pipe <- dbPipe conf
  let app = appName conf
  ExceptT $
    liftIO $
    runExceptT $
    insertUserPosts
      replace
      input
      (confGetDb app conf, pipe)
      (esServer conf, confGetEsIndex app conf)

-- ^
-- Update multiple records
updateMultiple :: Bool -> [Record] -> Api ApiResults
updateMultiple replace input = mkApiResponse return (createMultiple' replace input)

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
  :: (MonadReader ApiConfig m, MonadIO m, MonadBaseControl IO m)
  => Bool -> Text -> Record -> ExceptT ApiError m ApiResult
updateSingle' replace uid input = do
  result <- runEs (getByIds [uid] mappingName)
  existing <- ExceptT $ return (extract result)
  createSingle' replace (merge existing input)
  where
    merge old new = mergeRecords new (includeFields include old)
    include = ["postId", "subscriptionId"]
    extract r = maybe (Left mkApiError404) Right (extractRecord r)

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
