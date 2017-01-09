{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Handlers for Xandar endpoints
module Handlers.Users.Xandar where

import Api.Xandar
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (encode)
import Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Digest.Pure.SHA
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Database.MongoDB (Pipe, Collection, Database)
import Database.MongoDB.Query (Failure(..))
import Network.HTTP.Types.Status
import Persistence.Common
import Persistence.MongoDB
import Persistence.Users.Xandar (userDefinition, userIndices)
import Servant
import Types.Common

-- |
-- Application name
appName :: AppName
appName = "xandar"

-- |
-- MongoDB collection name
userColl :: Collection
userColl = "users"

-- |
-- Get the configured database name for this app
dbName :: ApiConfig -> Database
dbName = confGetDb appName

-- |
-- Create a MongoDB connection pipe
dbPipe
  :: MonadIO m
  => ApiConfig -> m Pipe
dbPipe conf = liftIO $ mkPipe (mongoHost conf) (mongoPort conf)

-- |
-- Create an application for providing the user functionality
app :: ApiConfig -> Application
app config = serve apiProxy (server config)
  where
    server :: ApiConfig -> Server XandarApi
    server conf = enter (toHandler conf) handlers
    toHandler :: ApiConfig -> Api :~> Handler
    toHandler conf = Nat (`runReaderT` conf)
    handlers :: ServerT XandarApi Api
    handlers =
      getMultiple :<|>
      getSingle :<|>
      deleteSingle :<|>
      createSingleOrMultiple :<|>
      replaceSingle :<|>
      replaceMultiple :<|>
      modifySingle :<|>
      modifyMultiple :<|>
      headSingle :<|>
      headMultiple :<|>
      optionsSingle :<|>
      optionsMultiple

-- |
-- Perform any initialization to be done on server start
appInit :: ApiConfig -> IO ()
appInit conf = do
  pipe <- dbPipe conf
  mapM_ (flip (dbAddIndex $ dbName conf) pipe) userIndices

-- |
-- Get multiple users
getMultiple :: ServerT GetMultiple Api
getMultiple include query sort page perPage = do
  conf <- ask
  pipe <- dbPipe conf
  count <- dbCount (dbName conf) userColl pipe
  let pagination = paginate (fromMaybe 1 page) (fromMaybe 50 perPage) count
  let start = paginationStart pagination
  let limit = paginationLimit pagination
  case queryToDoc (fromMaybe "" query) of
    Left err -> throwApiError $ ApiError (LBS.pack err) status400
    Right filter -> do
      users <- dbFind (dbName conf) userColl filter sort' include' start limit pipe
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
    mkUrl page' = mkGetMultipleLink include query sort (Just page') perPage
    mkPaginationLinks pagination =
      uncurry mkRelLink . second mkUrl <$>
      [ ("next", paginationNext pagination)
      , ("last", paginationLast pagination)
      , ("first", paginationFirst pagination)
      , ("prev", paginationPrev pagination)
      ]

-- |
-- Get a single user by id
getSingle :: Maybe Text -> Text -> Api (Headers '[Header "ETag" String] Record)
getSingle etag uid = do
  user <- getSingle' uid
  let sha = recToSha user
  let res = addHeader sha user
  case etag of
    Nothing -> return res
    Just tag ->
      if tag == T.pack sha
        then throwError err304
        else return res

-- |
-- Delete a single user
deleteSingle :: Text -> Api NoContent
deleteSingle uid = do
  _ <- getSingle' uid
  conf <- ask
  pipe <- dbPipe conf
  dbDeleteById (dbName conf) userColl uid pipe >> return NoContent

-- |
-- Create one or more users
createSingleOrMultiple
  :: ApiData Record
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingleOrMultiple (Single r) = createSingle r
createSingleOrMultiple (Multiple rs) = createMultiple rs

-- |
-- Create a single user
createSingle
  :: Record
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingle input = do
  user <- insertSingle input
  apiItem throwApiError (return . mkResult) user
  where
    mkResult r = addHeader (getUserLink r) $ noHeader (Single $ Succ r)

-- |
-- Create multiple users
createMultiple
  :: [Record]
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createMultiple inputs = do
  users <- mapM insertSingle inputs
  let links = apiItem mempty (mkLink . getUserLink) <$> users
  return . noHeader $ addHeader (intercalate ", " links) (Multiple users)

-- |
-- Update (replace) a single user
replaceSingle :: Text -> Record -> Api Record
replaceSingle = updateSingle True

-- |
-- Update (modify) a single user
modifySingle :: Text -> Record -> Api Record
modifySingle = updateSingle False

-- |
-- Update (replace) multiple users
replaceMultiple :: [Record] -> Api [ApiResult]
replaceMultiple = updateMultiple True

-- |
-- Update (modify) multiple users
modifyMultiple :: [Record] -> Api [ApiResult]
modifyMultiple = updateMultiple False

-- |
-- Handle a head request for a single user endpoint
headSingle
  :: Text
  -> Api (Headers '[Header "ETag" String] NoContent)
headSingle uid = do
  user <- getSingle' uid
  return $ addHeader (recToSha user) NoContent

-- |
-- Handle a head request for a multiple users endpoint
headMultiple :: Api (Headers '[Header "X-Total-Count" String] NoContent)
headMultiple = do
  conf <- ask
  pipe <- dbPipe conf
  count <- dbCount (dbName conf) userColl pipe
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
insertSingle :: Record -> Api ApiResult
insertSingle = chainResult (insertOrUpdateSingle dbInsert) . validateUser

-- |
-- Modify or replace a single user
updateSingle :: Bool -> Text -> Record -> Api Record
updateSingle replace uid user =
  updateSingle' replace uid user >>= apiItem throwApiError return

-- |
-- Modify or replace multiple users
updateMultiple :: Bool -> [Record] -> Api [ApiResult]
updateMultiple replace = mapM modify
  where
    modify u =
      chainResult
        (updateSingle' replace $ fromJust (getIdValue u))
        (vResultToApiItem $ validateHasId u)

-- |
-- Modify or replace a single user
updateSingle' :: Bool -> Text -> Record -> Api ApiResult
updateSingle' replace uid updated = do
  existing <- getSingle' uid
  chainResult
    (insertOrUpdateSingle dbUpdate)
    (validateUser $ merge existing updated)
  where
    merge =
      if replace
        then replaceRecords ["_createdAt", "_updatedAt", "_id"]
        else mergeRecords

-- |
-- Insert or update a valid @user@ record according to @action@
insertOrUpdateSingle
  :: (Database -> Collection -> Record -> Pipe -> Api (Either Failure RecordId))
  -> Record
  -> Api ApiResult
insertOrUpdateSingle action user = do
  conf <- ask
  pipe <- dbPipe conf
  result <- action (dbName conf) userColl (populateUserDefaults user) pipe
  case result of
    Left err -> return $ Fail (failedToApiError err)
    Right uid -> Succ . fromJust <$> dbGetById (dbName conf) userColl uid pipe

-- |
-- Get a user by id
getSingle' :: Text -> Api Record
getSingle' uid = do
  conf <- ask
  pipe <- dbPipe conf
  user <- dbGetById (dbName conf) userColl uid pipe
  maybe (throwError err404) return user

-- |
-- Create a link for a user resource
getUserLink :: Record -> String
getUserLink = mkGetSingleLink . fromJust . getIdValue

-- |
-- Validate a user record
validateUser :: Record -> ApiResult
validateUser = vResultToApiItem . validate userDefinition

-- |
-- Populate all missing fields of a user record with default values
populateUserDefaults :: Record -> Record
populateUserDefaults = populateDefaults userDefinition

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