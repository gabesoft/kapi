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
import Data.Aeson (encode)
import Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (intercalate)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Database.MongoDB (Pipe, Database, Index)
import Database.MongoDB.Query (Failure(..))
import Network.HTTP.Types.Status
import Persistence.Common
import Persistence.MongoDB
import Servant
import Servant.Utils.Enter (Enter)
import Types.Common

-- ^
-- Create an application that provides CRUD functionality for a record type
app'
  :: (Enter a (Api :~> Handler) (ServerT api Handler), HasServer api '[])
  => Proxy api -> a -> ApiConfig -> Application
app' proxy handlers = serve proxy . server' handlers

server'
  :: Enter a (Api :~> Handler) b
  => a -> ApiConfig -> b
server' handlers config = enter (toHandler config) handlers
  where
    toHandler :: ApiConfig -> Api :~> Handler
    toHandler conf = Nat (`runReaderT` conf)

-- ^
-- Get multiple records
getMultiple :: ApiGetMultipleLink -> RecordDefinition -> ServerT GetMultiple Api
getMultiple getLink def include query sort page perPage = do
  count <- runDb (dbCount def)
  let pagination = mkPagination page perPage count
  let start = paginationStart pagination
  let limit = paginationLimit pagination
  case queryToDoc (fromMaybe "" query) of
    Left err -> throwApiError $ mkApiError status400 err
    Right filter' -> do
      records <- runDb $ dbFind def filter' sort' include' start limit
      return $ mkGetMultipleResult records pagination mkUrl
  where
    sort' = splitFields mkSortField sort
    include' = splitFields mkIncludeField include
    mkUrl page' = getLink include query sort (Just page') perPage

-- ^
-- Get a single record by id
getSingle :: RecordDefinition -> Maybe Text -> Text -> Api (Headers '[Header "ETag" String] Record)
getSingle def etag uid = do
  record <- getSingle' def uid
  let sha = recToSha record
  let res = addHeader sha record
  case etag of
    Nothing -> return res
    Just tag ->
      if tag == T.pack sha
        then throwError err304
        else return res

-- ^
-- Delete a single record
deleteSingle :: RecordDefinition -> Text -> Api NoContent
deleteSingle def uid =
  getSingle' def uid >> runDb (dbDeleteById def uid) >> return NoContent

-- ^
-- Create one or more records
createSingleOrMultiple
  :: RecordDefinition -> (Text -> String) -> ApiData Record
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingleOrMultiple def getLink (Single r) = createSingle def getLink r
createSingleOrMultiple def getLink (Multiple rs) = createMultiple def getLink rs

-- ^
-- Create a single record
createSingle
  :: RecordDefinition
  -> (Text -> String)
  -> Record
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingle def getLink input = do
  record <- insertSingle def input
  apiItem throwApiError (return . mkResult) record
  where
    mkResult r = addHeader (getCreateLink getLink r) $ noHeader (Single $ Succ r)

-- ^
-- Create multiple records
createMultiple
  :: RecordDefinition
  -> (Text -> String)
  -> [Record]
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createMultiple def getLink inputs = do
  records <- mapM (insertSingle def) inputs
  let links = apiItem (const "<>") (mkLink . getCreateLink getLink) <$> records
  return . noHeader $ addHeader (intercalate ", " links) (Multiple records)

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
replaceMultiple :: RecordDefinition -> [Record] -> Api [ApiResult]
replaceMultiple def = updateMultiple def True

-- ^
-- Update (modify) multiple records
modifyMultiple :: RecordDefinition -> [Record] -> Api [ApiResult]
modifyMultiple def = updateMultiple def False

-- ^
-- Handle a head request for a single record endpoint
headSingle
  :: RecordDefinition -> Text
  -> Api (Headers '[Header "ETag" String] NoContent)
headSingle def uid = do
  record <- getSingle' def uid
  return $ addHeader (recToSha record) NoContent

-- ^
-- Handle a head request for a multiple records endpoint
headMultiple :: RecordDefinition -> Api (Headers '[Header "X-Total-Count" String] NoContent)
headMultiple def = flip addHeader NoContent . show <$> runDb (dbCount def) 

-- ^
-- Handle an options request for a single record endpoint
optionsSingle :: Text
              -> Api (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
optionsSingle _ = return $ addHeader "GET, PATCH, PUT, DELETE" NoContent

-- ^
-- Handle an options request for a multiple record endpoint
optionsMultiple :: Api (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
optionsMultiple = return $ addHeader "GET, POST, PATCH, PUT" NoContent

-- ^
-- Insert a single valid record
-- populating any missing fields with default values
insertSingle :: RecordDefinition -> Record -> Api ApiResult
insertSingle def =
  chainResult (insertOrUpdateSingle def dbInsert) . validateRecord def

-- ^
-- Modify or replace a single record
updateSingle :: RecordDefinition -> Bool -> Text -> Record -> Api Record
updateSingle def replace uid record =
  updateSingle' def replace uid record >>= apiItem throwApiError return

-- ^
-- Modify or replace multiple records
updateMultiple :: RecordDefinition -> Bool -> [Record] -> Api [ApiResult]
updateMultiple def replace = mapM modify
  where
    modify u =
      chainResult
        (updateSingle' def replace $ fromJust (getIdValue u))
        (vResultToApiItem $ validateHasId u)

-- ^
-- Modify or replace a single record
updateSingle' :: RecordDefinition -> Bool -> Text -> Record -> Api ApiResult
updateSingle' def replace uid updated = do
  existing <- getSingle' def uid
  chainResult
    (insertOrUpdateSingle def dbUpdate)
    (validateRecord def $ merge existing updated)
  where
    merge =
      if replace
        then replaceRecords [createdAtLabel, updatedAtLabel, idLabel]
        else mergeRecords

-- ^
-- Insert or update a valid @record@ record according to @action@
insertOrUpdateSingle
  :: RecordDefinition -> (RecordDefinition -> Record -> Database -> Pipe -> Api (Either Failure RecordId))
  -> Record
  -> Api ApiResult
insertOrUpdateSingle def action record = do
  result <- runDb $ action def (populateDefaults def record)
  case result of
    Left err -> return $ Fail (failedToApiError err)
    Right uid -> Succ . fromJust <$> runDb (dbGetById def uid)

-- ^
-- Get a record by id
getSingle' :: RecordDefinition -> Text -> Api Record
getSingle' def uid = do
  record <- runDb (dbGetById def uid)
  maybe (throwError err404) return record

-- ^
-- Add database indices
addDbIndices :: [Index] -> ApiConfig -> IO ()
addDbIndices indices conf = do
  pipe <- dbPipe conf
  mapM_ (\idx -> dbAddIndex idx (dbName conf) pipe) indices

-- ^
-- Run a database action
runDb ::
  (MonadReader ApiConfig m, MonadIO m) =>
  (Database -> Pipe -> m b) -> m b
runDb action = do
  conf <- ask
  pipe <- dbPipe conf
  action (dbName conf) pipe

-- ^
-- Get the configured database name for this app
dbName :: ApiConfig -> Database
dbName = confGetDb appName

-- ^
-- Create a MongoDB connection pipe
dbPipe
  :: MonadIO m
  => ApiConfig -> m Pipe
dbPipe conf = liftIO $ mkPipe (mongoHost conf) (mongoPort conf)

-- ^
-- Convert a MongoDB @Failure@ into an @ApiError@
-- TODO parse the MongoDB error object (the error is in BSON format)
--      https://docs.mongodb.com/manual/reference/command/getLastError/
failedToApiError :: Failure -> ApiError
failedToApiError (WriteFailure _ msg) = mkApiError status400 msg
failedToApiError err = ApiError (LBS.pack (show err)) status500

-- ^
-- Convert an 'ApiError' into a 'ServantErr' and throw
throwApiError
  :: MonadError ServantErr m
  => ApiError -> m a
throwApiError err
  | apiErrorStatus err == status400 = throwError (mkErr err400 err)
  | apiErrorStatus err == status404 = throwError (mkErr err404 err)
  | otherwise = throwError (mkErr err500 err)
  where
    mkErr sErr aErr =
      sErr {errBody = encode (Fail aErr :: ApiItem ApiError ())}

-- ^
-- Create an 'ApiError'
mkApiError :: Status -> String -> ApiError
mkApiError status msg = ApiError (LBS.pack msg) status

-- ^
-- Chain two API results
chainResult
  :: Monad m
  => (Record -> m ApiResult) -> ApiResult -> m ApiResult
chainResult = apiItem (return . Fail)

-- ^
-- Create a result for the 'getMultiple' endpoint
mkGetMultipleResult
  :: (AddHeader h4 String a orig3
     ,AddHeader h3 String orig3 orig2
     ,AddHeader h2 String orig2 orig1
     ,AddHeader h1 String orig1 orig
     ,AddHeader h String orig b)
  => a -> Pagination -> (Int -> String) -> b
mkGetMultipleResult records pagination mkUrl =
  header paginationPage $
  header paginationTotal $
  header paginationLast $
  header paginationSize $ mkLinkHeader pagination mkUrl records
  where
    header f = addHeader (show $ f pagination)

-- ^
-- Create a 'Pagination' object
mkPagination :: Maybe Int -> Maybe Int -> Int -> Pagination
mkPagination page perPage = paginate (fromMaybe 1 page) (fromMaybe perPageDefault perPage)

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
mkLinkHeader ::
  AddHeader h String a b =>
  Pagination -> (Int -> String) -> a -> b
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
-- Split a list of comma separated fields, apply a function to each one
-- and return the results that have a value
splitFields ::
  (Foldable t, Functor t) => (Text -> Maybe a) -> t Text -> [a]
splitFields f input = catMaybes $ f <$> concat (T.splitOn "," <$> input)

-- ^
-- Validate a record given a definition
validateRecord :: RecordDefinition -> Record -> ApiResult
validateRecord def = vResultToApiItem . validate def
