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
       (throwApiError, mkApiError, mkPagination, splitFields,
        mkLinkHeader, mkGetMultipleResult)
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
getSingle etag uid = do
  records <- runExceptT (runEs $ getByIds [uid])
  either throwApiError respond records
  where
    respond res = maybe (throwError err404) success (extractRecord res)
    checkEtag sha res tag
      | tag == T.pack sha = throwError err304
      | otherwise = return res
    success record =
      let sha = recToSha record
          res = addHeader sha record
      in maybe (return res) (checkEtag sha res) etag

-- ^
-- Get multiple records
getMultiple :: ServerT GetMultiple Api
getMultiple include query sortFields page perPage = do
  result <- runExceptT $ getMultiple' include query sortFields page perPage
  case result of
    Left err -> throwApiError err
    Right (records, pagination) ->
      return $ mkGetMultipleResult records pagination mkUrl
  where
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
  search <- getSearch (paginationStart pagination) (paginationLimit pagination)
  results <- runEs . searchDocuments $ search
  return (extractRecords include' results, pagination)
  where
    getCountSearch = toResult $ prepareSearch include' query sort' 0 0
    getSearch start = toResult . prepareSearch [] query sort' start
    mkFields = splitFields Just
    toResult = ExceptT . return . first (mkApiError status400)
    include' = mkFields include
    sort' = mkFields sortFields

-- ^
-- Run an elastic-search action
runEs
  :: (MonadReader ApiConfig m, MonadIO m)
  => (Text -> Text -> Text -> IO (Either EsError b)) -> ExceptT ApiError m b
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
  -> Either String Search
prepareSearch include query sortFields start limit = toSearch <$> expr
  where
    expr = sequence $ first ("Invalid query: " ++) . parse <$> query
    toSearch e = mkSearch e sortFields include start limit

-- ^
-- Convert an 'EsError' error into an 'ApiError'
esToApiError :: EsError -> ApiError
esToApiError err =
  ApiError
    (LBS.pack . T.unpack $ B.errorMessage err)
    (intToStatus $ B.errorStatus err)

-- ^
-- Convert an 'Int' to an HTTP 'Status'
intToStatus :: Int -> Status
intToStatus 400 = status400
intToStatus 403 = status403
intToStatus 404 = status404
intToStatus 500 = status500
intToStatus code = error $ "unknown status code " ++ show code

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

-- ^
-- Delete a single record
deleteSingle :: Text -> Api NoContent
deleteSingle uid = undefined

-- ^
-- Create one or more records
createSingleOrMultiple
  :: ApiData Record
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingleOrMultiple (Single r) = undefined
createSingleOrMultiple (Multiple rs) = undefined

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
-- Handle a head request for a single record endpoint
headSingle :: Text -> Api (Headers '[Header "ETag" String] NoContent)
headSingle uid = undefined

-- ^
-- Handle a head request for a multiple records endpoint
headMultiple :: Api (Headers '[Header "X-Total-Count" String] NoContent)
headMultiple = undefined

-- ^
-- Handle an options request for a single record endpoint
optionsSingle :: Text
              -> Api (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
optionsSingle _ = undefined

-- ^
-- Handle an options request for a multiple record endpoint
optionsMultiple :: Api (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
optionsMultiple = undefined
