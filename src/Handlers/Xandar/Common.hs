{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- |
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
import Database.MongoDB (Pipe, Database, Index, Collection)
import Database.MongoDB.Query (Failure(..))
import Network.HTTP.Types.Status
import Persistence.Common
import Persistence.MongoDB
import Servant
import Servant.Utils.Enter (Enter)
import Types.Common

-- |
-- Create an application for providing the user functionality
-- app
--   :: (HasServer a '[])
--   => Proxy a -> (ServerT a Api) -> ApiConfig -> Application
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

-- |
-- Get multiple users
getMultiple :: Collection -> ServerT GetMultiple Api
getMultiple coll include query sort page perPage = do
  conf <- ask
  pipe <- dbPipe conf
  count <- dbCount (dbName conf) coll pipe
  let pagination = paginate (fromMaybe 1 page) (fromMaybe 50 perPage) count
  let start = paginationStart pagination
  let limit = paginationLimit pagination
  case queryToDoc (fromMaybe "" query) of
    Left err -> throwApiError $ ApiError (LBS.pack err) status400
    Right filter' -> do
      users <-
        dbFind (dbName conf) coll filter' sort' include' start limit pipe
      return $
        addHeader (show $ paginationPage pagination) $
        addHeader (show $ paginationTotal pagination) $
        addHeader (show $ paginationLast pagination) $
        addHeader (show $ paginationSize pagination) $
        addHeader (intercalate "," $ mkPaginationLinks pagination) users
  where
    mkFields f input = catMaybes $ f <$> concat (T.splitOn "," <$> input)
    sort' = mkFields mkSortField sort
    include' = mkFields mkIncludeField include
    mkUrl page' = mkUserGetMultipleLink include query sort (Just page') perPage
    mkPaginationLinks pagination =
      uncurry mkRelLink . second mkUrl <$>
      [ ("next", paginationNext pagination)
      , ("last", paginationLast pagination)
      , ("first", paginationFirst pagination)
      , ("prev", paginationPrev pagination)
      ]

-- |
-- Get a single user by id
getSingle :: Collection -> Maybe Text -> Text -> Api (Headers '[Header "ETag" String] Record)
getSingle coll etag uid = do
  user <- getSingle' coll uid
  let sha = recToSha user
  let res = addHeader sha user
  case etag of
    Nothing -> return res
    Just tag ->
      if tag == T.pack sha
        then throwError err304
        else return res

-- |
-- Delete a single record
deleteSingle :: Collection -> Text -> Api NoContent
deleteSingle coll uid = do
  _ <- getSingle' coll uid
  conf <- ask
  pipe <- dbPipe conf
  dbDeleteById (dbName conf) coll uid pipe >> return NoContent

-- |
-- Create one or more records
createSingleOrMultiple
  :: RecordDefinition -> Collection -> ApiData Record
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingleOrMultiple def coll (Single r) = createSingle def coll r
createSingleOrMultiple def coll (Multiple rs) = createMultiple def coll rs

-- |
-- Create a single record
createSingle
  :: RecordDefinition
  -> Collection
  -> Record
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingle def coll input = do
  user <- insertSingle def coll input
  apiItem throwApiError (return . mkResult) user
  where
    mkResult r = addHeader (getUserLink r) $ noHeader (Single $ Succ r)

-- |
-- Create multiple users
createMultiple
  :: RecordDefinition -> Collection -> [Record]
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createMultiple def coll inputs = do
  users <- mapM (insertSingle def coll) inputs
  let links = apiItem (const "<>") (mkLink . getUserLink) <$> users
  return . noHeader $ addHeader (intercalate ", " links) (Multiple users)

-- |
-- Update (replace) a single user
replaceSingle :: RecordDefinition -> Collection -> Text -> Record -> Api Record
replaceSingle def coll = updateSingle def coll True

-- |
-- Update (modify) a single user
modifySingle :: RecordDefinition -> Collection -> Text -> Record -> Api Record
modifySingle def coll = updateSingle def coll False

-- |
-- Update (replace) multiple users
replaceMultiple :: RecordDefinition -> Collection -> [Record] -> Api [ApiResult]
replaceMultiple def coll = updateMultiple def coll True

-- |
-- Update (modify) multiple users
modifyMultiple :: RecordDefinition -> Collection -> [Record] -> Api [ApiResult]
modifyMultiple def coll = updateMultiple def coll False

-- |
-- Handle a head request for a single user endpoint
headSingle
  :: Collection -> Text
  -> Api (Headers '[Header "ETag" String] NoContent)
headSingle coll uid = do
  user <- getSingle' coll uid
  return $ addHeader (recToSha user) NoContent

-- |
-- Handle a head request for a multiple users endpoint
headMultiple :: Collection -> Api (Headers '[Header "X-Total-Count" String] NoContent)
headMultiple coll = do
  conf <- ask
  pipe <- dbPipe conf
  count <- dbCount (dbName conf) coll pipe
  return $ addHeader (show count) NoContent

-- |
-- Handle an options request for a single user endpoint
optionsSingle :: Text
              -> Api (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
optionsSingle _ = return $ addHeader "GET, PATCH, PUT, DELETE" NoContent

-- |
-- Handle an options request for a multiple user endpoint
optionsMultiple :: Api (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
optionsMultiple = return $ addHeader "GET, POST, PATCH, PUT" NoContent

-- |
-- Insert a single valid user
-- populating any missing fields with default values
insertSingle :: RecordDefinition -> Collection -> Record -> Api ApiResult
insertSingle def coll =
  chainResult (insertOrUpdateSingle def coll dbInsert) . validateRecord def

-- |
-- Modify or replace a single user
updateSingle :: RecordDefinition -> Collection -> Bool -> Text -> Record -> Api Record
updateSingle def coll replace uid user =
  updateSingle' def coll replace uid user >>= apiItem throwApiError return

-- |
-- Modify or replace multiple users
updateMultiple :: RecordDefinition -> Collection -> Bool -> [Record] -> Api [ApiResult]
updateMultiple def coll replace = mapM modify
  where
    modify u =
      chainResult
        (updateSingle' def coll replace $ fromJust (getIdValue u))
        (vResultToApiItem $ validateHasId u)

-- |
-- Modify or replace a single user
updateSingle' :: RecordDefinition -> Collection -> Bool -> Text -> Record -> Api ApiResult
updateSingle' def coll replace uid updated = do
  existing <- getSingle' coll uid
  chainResult
    (insertOrUpdateSingle def coll dbUpdate)
    (validateRecord def $ merge existing updated)
  where
    merge =
      if replace
        then replaceRecords ["_createdAt", "_updatedAt", "_id"]
        else mergeRecords

-- |
-- Insert or update a valid @user@ record according to @action@
insertOrUpdateSingle
  :: RecordDefinition -> Collection -> (Database -> Collection -> Record -> Pipe -> Api (Either Failure RecordId))
  -> Record
  -> Api ApiResult
insertOrUpdateSingle def coll action user = do
  conf <- ask
  pipe <- dbPipe conf
  result <- action (dbName conf) coll (populateDefaults def user) pipe
  case result of
    Left err -> return $ Fail (failedToApiError err)
    Right uid -> Succ . fromJust <$> dbGetById (dbName conf) coll uid pipe

-- |
-- Get a user by id
getSingle' :: Collection -> Text -> Api Record
getSingle' coll uid = do
  conf <- ask
  pipe <- dbPipe conf
  user <- dbGetById (dbName conf) coll uid pipe
  maybe (throwError err404) return user

-- |
-- Get the configured database name for this app
dbName :: ApiConfig -> Database
dbName = confGetDb appName

-- |
-- Add database indices
addDbIndices :: [Index] -> ApiConfig -> IO ()
addDbIndices indices conf = do
  pipe <- dbPipe conf
  mapM_ (flip (dbAddIndex $ dbName conf) pipe) indices

-- |
-- Create a MongoDB connection pipe
dbPipe
  :: MonadIO m
  => ApiConfig -> m Pipe
dbPipe conf = liftIO $ mkPipe (mongoHost conf) (mongoPort conf)

-- |
-- Convert a MongoDB @Failure@ into an @ApiError@
-- TODO parse the MongoDB error object (the error is in BSON format)
--      https://docs.mongodb.com/manual/reference/command/getLastError/
failedToApiError :: Failure -> ApiError
failedToApiError (WriteFailure _ msg) = ApiError (LBS.pack msg) status400
failedToApiError err = ApiError (LBS.pack (show err)) status500

-- |
-- Convert an @ApiError@ into a @ServantErr@ and throw
throwApiError
  :: MonadError ServantErr m
  => ApiError -> m a
throwApiError err
  | apiErrorStatus err == status400 = throwError (mkErr err400 err)
  | apiErrorStatus err == status404 = throwError (mkErr err404 err)
  | otherwise = throwError (mkErr err500 err)
  where
    mkErr sErr aErr =
      sErr
      { errBody = encode (Fail aErr :: ApiItem ApiError ())
      }

-- |
-- Chain two API results
chainResult
  :: Monad m
  => (Record -> m ApiResult) -> ApiResult -> m ApiResult
chainResult = apiItem (return . Fail)

-- |
-- Create a link element
mkLink :: String -> String
mkLink link = "<" ++ link ++ ">"

-- |
-- Create a relative link element
mkRelLink :: String -> String -> String
mkRelLink rel link = mkLink link ++ "; rel=\"" ++ rel ++ "\""

-- |
-- Create a link for a user resource
getUserLink :: Record -> String
getUserLink = mkUserGetSingleLink . fromJust . getIdValue

-- |
-- Validate a user record
validateRecord :: RecordDefinition -> Record -> ApiResult
validateRecord def = vResultToApiItem . validate def