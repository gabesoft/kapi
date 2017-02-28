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
import Data.Bson ((=:))
import Data.Text (Text)
import Database.Bloodhound (Search, EsError)
import Handlers.Xandar.Common
       (mkPagination, splitLabels, mkGetMultipleResult,
        mkCreateMultipleResult, getCreateLink, mkGetSingleResult,
        mkCreateSingleResult, runSingle, updateSingle, runMulti)
import qualified Handlers.Xandar.Common as C
import Parsers.Filter (parse)
import Persistence.Common
import Persistence.ElasticSearch
import Persistence.Facade (runEs, dbPipe)
import Persistence.Xandar.Common (userPostDefinition)
import Persistence.Xandar.UserPosts
       (esDelete, esInsert, esInsertMulti, esReplace, esReplaceMulti,
        esModify, esModifyMulti)
import Servant
import Types.Common
import Util.Error

-- ^
-- Elastic-search mapping name
mappingName :: Text
mappingName = recordCollection userPostDefinition

-- ^
-- Get a single record by id
getSingle :: Maybe Text -> Text -> Api (Headers '[ Header "ETag" String] Record)
getSingle etag uid = runSingle (runEs $ getByIds [uid] mappingName) respond
  where
    respond = maybe (throwError err404) (mkGetSingleResult etag) . extractRecord

-- ^
-- Get multiple records
getMultiple :: ApiGetMultipleLink -> ServerT GetMultiple Api
getMultiple getLink include query sortFields page perPage =
  runSingle getRecords (return . respond)
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
deleteSingle uid = runSingle (esDelete uid) (const $ return NoContent)

-- ^
-- Create one or more records
createSingleOrMultiple
  :: (Text -> String)
  -> ApiData Record
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingleOrMultiple getLink (Single r) = createSingle getLink r
createSingleOrMultiple getLink (Multiple rs) = createMultiple getLink rs

-- ^
-- Create a single record
createSingle
  :: (Text -> String)
  -> Record
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingle getLink input =
  runSingle (esInsert input) (mkCreateSingleResult getLink)

-- ^
-- Create multiple records
createMultiple
  :: (Text -> String)
  -> [Record]
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createMultiple getLink input =
  runMulti (esInsertMulti input) >>= mkCreateMultipleResult getLink

-- ^
-- Update (replace) a single record
replaceSingle :: Text -> Record -> Api Record
replaceSingle = updateSingle esReplace

-- ^
-- Update (modify) a single record
modifySingle :: Text -> Record -> Api Record
modifySingle = updateSingle esModify

-- ^
-- Update (replace) multiple records
replaceMultiple :: [Record] -> Api [ApiResult]
replaceMultiple = runMulti . esReplaceMulti

-- ^
-- Update (modify) multiple records
modifyMultiple :: [Record] -> Api [ApiResult]
modifyMultiple = runMulti . esModifyMulti

-- ^
-- Handle an options request for a single record endpoint
optionsSingle :: Text
              -> Api (Headers '[ Header "Access-Control-Allow-Methods" String] NoContent)
optionsSingle = C.optionsSingle

-- ^
-- Handle an options request for a multiple record endpoint
optionsMultiple :: Api (Headers '[ Header "Access-Control-Allow-Methods" String] NoContent)
optionsMultiple = C.optionsMultiple

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
    toErr msg = mk400Err "Invalid query" $ Record ["query" =: msg]
