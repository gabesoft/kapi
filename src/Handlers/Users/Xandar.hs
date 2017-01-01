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

appName :: AppName
appName = "xandar"

dbName :: ApiConfig -> Database
dbName = confGetDb appName

userColl :: Collection
userColl = "users"

dbPipe
  :: MonadIO m
  => ApiConfig -> m Pipe
dbPipe cfg = liftIO $ mkPipe (mongoHost cfg) (mongoPort cfg)

-- |
-- Create a application for providing the user functionality
app :: ApiConfig -> Application
app config = serve apiProxy (server config)
  where
    server :: ApiConfig -> Server XandarApi
    server cfg = enter (toHandler cfg) handlers
    toHandler :: ApiConfig -> Api :~> Handler
    toHandler cfg = Nat (`runReaderT` cfg)

appInit :: ApiConfig -> IO ()
appInit conf = do
  pipe <- dbPipe conf
  mapM_ (flip (dbAddIndex $ dbName conf) pipe) userIndices

-- |
-- Get multiple users
getMultiple :: ServerT GetMultiple Api
getMultiple fields query sort start limit
                                    -- TODO add sorting, pagination, etc
 = do
  conf <- ask
  users <- dbPipe conf >>= dbFind (dbName conf) userColl
  return $ addHeader "pagination links" (addHeader (show $ length users) users)

-- |
-- Get a single user by id
getSingle :: Maybe Text -> Text -> Api (Headers '[Header "ETag" String] Record)
getSingle etag uid = do
  user <- getSingle' uid
  let sha = showDigest (recToSha user)
  let res = addHeader (showDigest $ recToSha user) user
  case etag of
    Nothing -> return res
    Just tag -> if tag == T.pack sha then throwError err304 else return res

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
createSingle input = validateAndRun input (insertSingle >=> mkResult)
  where
    mkResult (Succ r) = return $ addHeader (getUserLink r) $ noHeader (mkData r)
    mkResult (Fail e) = throwApiError e
    mkData = Single . Succ

-- |
-- Create multiple users
createMultiple
  :: [Record]
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createMultiple inputs = do
  users <- mapM insert (validateUser <$> inputs)
  let links = apiItem mempty (mkLink . getUserLink) <$> users
  return . noHeader $ addHeader (intercalate ", " links) (Multiple users)
  where
    mkLink path = "<" ++ path ++ ">"
    insert = chainResult insertSingle

-- |
-- Update (replace) a single user
replaceSingle :: Text -> Record -> Api Record
replaceSingle uid input =
  getSingle' uid >> validateAndRun user (updateSingle >=> mkResult)
  where
    user = setIdValue uid input
    mkResult (Fail e) = throwApiError e
    mkResult (Succ r) = return r

-- |
-- Update (replace) multiple users
replaceMultiple :: [Record] -> Api [ApiResult]
replaceMultiple input =
  mapM (chainResult updateSingle) (validateUserWithId <$> input)

-- |
-- Update (modify) a single user
modifySingle :: Text -> Record -> Api Record
modifySingle uid user = do
  existing <- getSingle' uid
  validateAndRun
    (existing <> user)
    (updateSingle >=> apiItem throwApiError return)

-- |
-- Update (modify) multiple users
modifyMultiple :: [Record] -> Api [ApiResult]
modifyMultiple input =
  mapM (chainResult modify) (vResultToApiItem . validateHasId <$> input)
  where
    modify :: Record -> Api ApiResult
    modify u = do
      current <- getSingle' (fromJust $ getIdValue u)
      chainResult updateSingle (validateUser $ current <> u)

-- |
-- Handle a head request for a single user endpoint
headSingle
  :: Text
  -> Api (Headers '[Header "ETag" String] NoContent)
headSingle uid = do
  user <- getSingle' uid
  return $ addHeader (showDigest $ recToSha user) NoContent

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
optionsSingle :: Text -> Api (Headers '[Header "Allow" String] NoContent)
optionsSingle = undefined

-- |
-- Insert a single valid user
-- populating any missing fields with default values
insertSingle :: Record -> Api ApiResult
insertSingle = insertOrUpdateSingle dbInsert

-- |
-- Update an existing valid user
-- populating any missing fields with default values
updateSingle :: Record -> Api ApiResult
updateSingle = insertOrUpdateSingle dbUpdate

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
-- Handle an options request for a multiple user endpoint
optionsMultiple :: Api (Headers '[Header "Allow" String] NoContent)
optionsMultiple = undefined

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
-- Validate a record and run an action if valid or throw an error
validateAndRun
  :: MonadError ServantErr m
  => Record -> (Record -> m a) -> m a
validateAndRun record act = apiItem throwApiError act (validateUser record)

-- |
-- Validate a user record
validateUser' :: Record -> (Record, ValidationResult)
validateUser' = validate userDefinition

-- |
-- Validate a user record
validateUser :: Record -> ApiResult
validateUser = vResultToApiItem . validateUser'

-- |
-- Validate a user record and ensure that it contains a valid id
validateUserWithId :: Record -> ApiResult
validateUserWithId user = vResultToApiItem $ valUser user
  where
    valUser u = second (mappend . snd $ validateHasId u) (validateUser' u)

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
    mkErr se ae =
      se
      { errBody = encode (Fail ae :: ApiItem ApiError ())
      }

chainResult
  :: Monad m
  => (Record -> m ApiResult) -> ApiResult -> m ApiResult
chainResult = apiItem (return . Fail)
