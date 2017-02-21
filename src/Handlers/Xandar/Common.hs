{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- ^
-- Common handlers
module Handlers.Xandar.Common where

import Api.Xandar
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson (encode)
import Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (intercalate)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Database.MongoDB (Database, Index, Pipe, Failure)
import Network.HTTP.Types.Status
import Persistence.Common
import Persistence.Facade
       (runDb, dbInsert, dbInsertMulti, dbUpdate,
        dbUpdateMulti, getExisting, dbPipe)
import qualified Persistence.MongoDB as DB
import Servant
import Servant.Utils.Enter (Enter)
import Types.Common

-- ^
-- Create an application that provides CRUD functionality for a record type
app'
  :: (Enter a (Api :~> Handler) (ServerT api Handler), HasServer api '[])
  => Proxy api -> a -> ApiConfig -> Application
app' proxy handlers = serve proxy . server' handlers
  where
    server'
      :: Enter a (Api :~> Handler) b
      => a -> ApiConfig -> b
    server' hs cfg = enter (toHandler cfg) hs
    toHandler :: ApiConfig -> Api :~> Handler
    toHandler conf = Nat (`runReaderT` conf)

-- ^
-- Get a single record by id
getSingle
  :: RecordDefinition
  -> Maybe Text
  -> Text
  -> Api (Headers '[ Header "ETag" String] Record)
getSingle def etag uid = get return >>= mkGetSingleResult etag
  where
    get f = do
      result <- runExceptT $ getExisting appName def uid
      either throwApiError f result

-- ^
-- Get multiple records
getMultiple :: ApiGetMultipleLink -> RecordDefinition -> ServerT GetMultiple Api
getMultiple getLink def include query sort page perPage =
  mkApiResponse (return . respond) getRecords
  where
    getRecords = getMultiple' def include query sort page perPage
    mkUrl page' = getLink include query sort (Just page') perPage
    respond = uncurry (mkGetMultipleResult mkUrl)

-- ^
-- Helper for 'getMultiple'
getMultiple'
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => RecordDefinition
  -> [Text]
  -> Maybe Text
  -> [Text]
  -> Maybe Int
  -> Maybe Int
  -> ExceptT ApiError m ([Record], Pagination)
getMultiple' def include query sort page perPage = do
  query' <- ExceptT (return getQuery)
  count <- runDb' (DB.dbCount def query')
  let pagination = mkPagination page perPage count
  let start = paginationStart pagination
  let limit = paginationLimit pagination
  records <- runDb' $ DB.dbFind def query' sort' include' start limit
  return (records, pagination)
  where
    sort' = DB.mkSortFields (splitLabels sort)
    include' = DB.mkIncludeFields (splitLabels include)
    getQuery = first mkApiError400 (DB.queryToDoc def $ fromMaybe mempty query)

-- ^
-- Delete a single record
deleteSingle :: RecordDefinition -> Text -> Api NoContent
deleteSingle def uid = verify >> mkApiResponse (const $ return NoContent) delete
  where
    verify = getSingle def mempty uid
    delete = runDb' $ DB.dbDeleteById def uid

-- ^
-- Create one or more records
createSingleOrMultiple
  :: RecordDefinition
  -> (Text -> String)
  -> ApiData Record
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingleOrMultiple def getLink (Single r) = createSingle def getLink r
createSingleOrMultiple def getLink (Multiple rs) = createMultiple def getLink rs

-- ^
-- Create a single record
createSingle
  :: RecordDefinition
  -> (Text -> String)
  -> Record
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingle def getLink input = insert (mkCreateSingleResult getLink)
  where
    insert f = do
      result <- runExceptT $ dbInsert appName def input
      either throwApiError f result

-- ^
-- Create multiple records
createMultiple
  :: RecordDefinition
  -> (Text -> String)
  -> [Record]
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createMultiple def getLink input = insert >>= mkCreateMultipleResult getLink
  where
    insert = do
      records <- runApiItems2T $ dbInsertMulti appName def input
      return $ itemsToApiResults records

-- ^
-- Update (replace) a single record
replaceSingle :: RecordDefinition -> Text -> Record -> Api Record
replaceSingle def = updateSingle def True

-- ^
-- Update (modify) a single record
modifySingle :: RecordDefinition -> Text -> Record -> Api Record
modifySingle def = updateSingle def False

-- ^
-- Update (replace) multiple records
replaceMultiple :: RecordDefinition -> [Record] -> Api ApiResults
replaceMultiple def = updateMultiple def True

-- ^
-- Update (modify) multiple records
modifyMultiple :: RecordDefinition -> [Record] -> Api ApiResults
modifyMultiple def = updateMultiple def False

-- ^
-- Handle an options request for a single record endpoint
optionsSingle :: Text
              -> Api (Headers '[ Header "Access-Control-Allow-Methods" String] NoContent)
optionsSingle _ = return $ addHeader "GET, PATCH, PUT, DELETE" NoContent

-- ^
-- Handle an options request for a multiple record endpoint
optionsMultiple :: Api (Headers '[ Header "Access-Control-Allow-Methods" String] NoContent)
optionsMultiple = return $ addHeader "GET, POST, PATCH, PUT" NoContent

-- ^
-- Modify or replace a single record
updateSingle :: RecordDefinition -> Bool -> Text -> Record -> Api Record
updateSingle def replace uid input = update return
  where
    record = setIdValue uid input
    update f = do
      result <- runExceptT $ dbUpdate replace appName def record
      either throwApiError f result

-- ^
-- Modify or replace multiple records
updateMultiple :: RecordDefinition -> Bool -> [Record] -> Api ApiResults
updateMultiple def replace input = update
  where
    update = do
      records <- runApiItems2T $ dbUpdateMulti replace appName def input
      return $ itemsToApiResults records

-- ^
-- Add database indices
addDbIndices :: [Index] -> ApiConfig -> IO ()
addDbIndices indices conf = do
  pipe <- dbPipe conf
  mapM_ (\idx -> DB.dbAddIndex idx (dbName conf) pipe) indices

-- ^
-- Run a database action
runDb'
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => (Database -> Pipe -> m (Either Failure a)) -> ExceptT ApiError m a
runDb' = runDb appName

-- ^
-- Prepare an API response
mkApiResponse
  :: MonadError ServantErr m
  => (a -> m b) -> ExceptT ApiError m a -> m b
mkApiResponse f action = do
  result <- runExceptT action
  either throwApiError f result

-- ^
-- Get the configured database name for this app
dbName :: ApiConfig -> Database
dbName = confGetDb appName

-- ^
-- Elastic-search index
esIndex :: ApiConfig -> Text
esIndex = confGetEsIndex appName

-- ^
-- Convert an 'ApiError' into a 'ServantErr' and throw
throwApiError
  :: MonadError ServantErr m
  => ApiError -> m a
throwApiError err = throwError (mkServantErr err)

-- ^
-- Convert an 'ApiError' into a 'ServantErr'
mkServantErr :: ApiError -> ServantErr
mkServantErr err =
  ServantErr
  { errHTTPCode = statusCode status
  , errReasonPhrase = BS.unpack (statusMessage status)
  , errBody = body
  , errHeaders = []
  }
  where
    status = apiErrorStatus err
    msg = apiErrorMessage err
    body
      | LBS.null msg = mempty
      | otherwise = encode (Fail err :: ApiItem ApiError ())

-- ^
-- Create a result to be returned from a 'getSingle' method
mkGetSingleResult :: Maybe Text
                  -> Record
                  -> Api (Headers '[ Header "ETag" String] Record)
mkGetSingleResult etag record = maybe (return res) (checkEtag sha res) etag
  where
    sha = recToSha record
    res = addHeader sha record

-- ^
-- Create a result to be returned from a 'createSingle' method
mkCreateSingleResult
  :: (Text -> String)
  -> Record
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
mkCreateSingleResult getLink record =
  return $
  addHeader (getCreateLink getLink record) $ noHeader (Single $ Succ record)

-- ^
-- Create a result to be returned from a 'createMultiple' method
mkCreateMultipleResult
  :: (Text -> String)
  -> ApiResults
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
mkCreateMultipleResult getLink results =
  return . noHeader $
  addHeader (intercalate ", " $ links records) (Multiple records)
  where
    records = apiItems results
    links = fmap $ apiItem (const "<>") (mkLink . getCreateLink getLink)

-- ^
-- Create a result to be returned from a 'getMultiple' endpoint
mkGetMultipleResult
  :: ( AddHeader h4 String a orig3
     , AddHeader h3 String orig3 orig2
     , AddHeader h2 String orig2 orig1
     , AddHeader h1 String orig1 orig
     , AddHeader h String orig b
     )
  => (Int -> String) -> a -> Pagination -> b
mkGetMultipleResult mkUrl records pagination =
  header paginationPage $
  header paginationTotal $
  header paginationLast $
  header paginationSize $ mkLinkHeader pagination mkUrl records
  where
    header f = addHeader (show $ f pagination)

-- ^
-- Check an ETag against a SHA and throw a 304 error or return the output
checkEtag
  :: MonadError ServantErr m
  => String -> a -> Text -> m a
checkEtag sha output etag
  | etag == T.pack sha = throwError err304
  | otherwise = return output

-- ^
-- Create a 'Pagination' object
mkPagination :: Maybe Int -> Maybe Int -> Int -> Pagination
mkPagination page perPage =
  paginate (fromMaybe 1 page) (fromMaybe perPageDefault perPage)

-- ^
-- Create a set of pagination links to be returned in the 'Link' header
mkPaginationLinks :: Pagination -> (Int -> String) -> [String]
mkPaginationLinks pagination mkUrl =
  uncurry mkRelLink . second mkUrl <$>
  [ ("next", paginationNext pagination)
  , ("last", paginationLast pagination)
  , ("first", paginationFirst pagination)
  , ("prev", paginationPrev pagination)
  ]

-- ^
-- Create a 'Link' header containing pagination links
mkLinkHeader
  :: AddHeader h String a b
  => Pagination -> (Int -> String) -> a -> b
mkLinkHeader pagination mkUrl =
  addHeader (intercalate "," $ mkPaginationLinks pagination mkUrl)

-- ^
-- Create a link element
mkLink :: String -> String
mkLink link = "<" ++ link ++ ">"

-- ^
-- Create a relative link element
mkRelLink :: String -> String -> String
mkRelLink rel link = mkLink link ++ "; rel=\"" ++ rel ++ "\""

-- ^
-- Create a link to be returned during record creation
getCreateLink :: (Text -> String) -> Record -> String
getCreateLink getLink = getLink . fromJust . getIdValue

-- ^
-- Split a list o comma separated field names and remove all empty entries
splitLabels
  :: (Foldable t, Functor t)
  => t Text -> [Text]
splitLabels input = filter (not . T.null) $ concat (T.splitOn "," <$> input)
