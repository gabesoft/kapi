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
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Database.Bloodhound (SearchResult(..), EsError, Search)
import qualified Database.Bloodhound as B
import Handlers.Xandar.Common
       (throwApiError, mkPagination, splitFields, mkLinkHeader,
        mkGetMultipleResult, checkEtag, mkApiResponse, mkApiResult)
import Network.HTTP.Types.Status
import Parsers.Filter (parse)
import Persistence.Common
import Persistence.ElasticSearch
import Servant
import Types.Common

-- ^
-- Elastic-search mapping name
mappingName :: Text
mappingName = "post"

-- ^
-- Elastic-search index
esIndex :: ApiConfig -> Text
esIndex = confGetEsIndex appName

-- ^
-- Get a single record by id
getSingle :: Maybe Text -> Text -> Api (Headers '[Header "ETag" String] Record)
getSingle etag uid = mkApiResponse respond (runEs $ getByIds [uid])
  where
    respond res = maybe (throwError err404) success (extractRecord res)
    success record =
      let sha = recToSha record
          res = addHeader sha record
      in maybe (return res) (checkEtag sha res) etag

-- ^
-- Get multiple records
getMultiple :: ServerT GetMultiple Api
getMultiple include query sortFields page perPage =
  mkApiResponse (return . respond) getRecords
  where
    getRecords = getMultiple' include query sortFields page perPage
    respond (records, pagination) = mkGetMultipleResult mkUrl records pagination
    getLink = mkUserPostGetMultipleLink
    mkUrl page' = getLink include query sortFields (Just page') perPage

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
  count <- runEs . countDocuments =<< getCountSearch
  let pagination = mkPagination page perPage count
  let start = paginationStart pagination
  let limit = paginationLimit pagination
  search <- getSearch start limit
  results <- runEs . searchDocuments $ search
  return (extractRecords include' results, pagination)
  where
    getCountSearch = ExceptT . return $ prepareSearch include' query sort' 0 0
    getSearch start = ExceptT . return . prepareSearch [] query sort' start
    mkFields = splitFields Just
    include' = mkFields include
    sort' = mkFields sortFields

-- ^
-- Delete a single record
deleteSingle :: Text -> Api NoContent
deleteSingle uid = verify >> mkApiResponse (const $ return NoContent) delete
  where
    verify = getSingle mempty uid
    delete = runEs $ deleteDocument uid

-- ^
-- Create one or more records
createSingleOrMultiple
  :: ApiData Record
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingleOrMultiple (Single r) = createSingle r
createSingleOrMultiple (Multiple rs) = createMultiple rs

-- ^
-- Create a single record
createSingle
  :: Record
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingle input = undefined

-- ^
-- Create multiple records
createMultiple
  :: [Record]
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createMultiple inputs = undefined

createMultiple' inputs = undefined

-- ^
-- Update (replace) a single record
replaceSingle :: Text -> Record -> Api Record
replaceSingle = undefined

-- ^
-- Update (modify) a single record
modifySingle :: Text -> Record -> Api Record
modifySingle = undefined

-- ^
-- Update (replace) multiple records
replaceMultiple :: [Record] -> Api [ApiResult]
replaceMultiple = undefined

-- ^
-- Update (modify) multiple records
modifyMultiple :: [Record] -> Api [ApiResult]
modifyMultiple = undefined

-- ^
-- Handle an options request for a single record endpoint
optionsSingle :: Text
              -> Api (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
optionsSingle _ = undefined

-- ^
-- Handle an options request for a multiple record endpoint
optionsMultiple :: Api (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
optionsMultiple = undefined

-- ^
-- Run an elastic-search action
runEs
  :: (MonadReader ApiConfig m, MonadIO m)
  => (Text -> Text -> Text -> IO (Either EsError d)) -> ExceptT ApiError m d
runEs action = do
  conf <- ask
  let server = esServer conf
  let index = esIndex conf
  results <- liftIO $ action server index mappingName
  ExceptT (return $ first esToApiError results)

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

-- ^
-- Extract a 'Record' from a 'SearchResult'
extractRecord :: SearchResult Record -> Maybe Record
extractRecord results =
  case extractRecords [] results of
    [] -> Nothing
    x:_ -> Just x

-- ^
-- Extract all 'Record's from a 'SearchResult'
extractRecords :: [Text] -> SearchResult Record -> [Record]
extractRecords fields input = includeFields fields <$> records
  where
    result = B.searchHits input
    hits = B.hits result
    getRecord = catMaybes . (: []) . getRecord'
    getRecord' hit = setValue idLabel (getId $ B.hitDocId hit) <$> B.hitSource hit
    getId (B.DocId docId) = docId
    records = concat (getRecord <$> hits)
