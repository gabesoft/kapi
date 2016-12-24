{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for Xandar endpoints
module Handlers.Users.Xandar where

import Api.Xandar
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Function ((&))
import Data.Maybe
import Data.Text (Text)
import Database.MongoDB (Pipe, Collection, Database)
import Database.MongoDB.Query (Failure(..))
import Persistence.MongoDB
import Persistence.Users.Xandar (userDefinition, u1, u2)
import Servant
import Types.Common

type Api = ReaderT ApiConfig Handler

handlers :: ServerT XandarApi Api
handlers
 =
  getMultiple :<|>
  getSingle :<|>
  deleteSingle :<|>
  createSingle :<|>
  createMultiple :<|>
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

dbPipe :: ApiConfig -> Api Pipe
dbPipe cfg = liftIO $ mkPipe (mongoHost cfg) (mongoPort cfg)

validateUser :: Record -> ValidationResult
validateUser = validate userDefinition

populateUserDefaults :: Record -> Record
populateUserDefaults = populateDefaults userDefinition

-- |
-- Create a application for providing the user functionality
app :: ApiConfig -> Application
app config = serve apiProxy (server config)
  where
    server :: ApiConfig -> Server XandarApi
    server cfg = enter (toHandler cfg) handlers
    toHandler :: ApiConfig -> Api :~> Handler
    toHandler cfg = Nat (`runReaderT` cfg)

-- |
-- Get multiple users
getMultiple :: ServerT GetMultiple Api
getMultiple fields query sort start limit =
  return $ addHeader "pagination links" (addHeader "10" [u1, u2])

-- |
-- Get a single user by id
getSingle :: Text -> Api Record
getSingle uid = snd <$> getRecordAndLink uid >>= maybe (throw404 mempty) return

-- |
-- Delete a single user
deleteSingle :: Text -> Api NoContent
deleteSingle uid = do
  cfg <- ask
  _ <- getSingle uid
  dbPipe cfg >>= dbDeleteById (dbName cfg) userColl uid
  return NoContent

-- |
-- Create a single user
createSingle :: Record -> Api (Headers '[Header "Location" String] Record)
createSingle input =
  validateAndRun input ((uncurry addHeader <$>) . insertSingle)

-- |
-- Insert a single valid user
insertSingle :: Record -> Api (String, Record)
insertSingle input = do
  cfg <- ask
  pipe <- dbPipe cfg
  result <- dbInsert (dbName cfg) userColl (populateUserDefaults input) pipe
  either throwFailed ((second fromJust <$>) . getRecordAndLink) result

-- |
-- Get the record with @uid@ and its resource link
getRecordAndLink :: RecordId -> Api (String, Maybe Record)
getRecordAndLink uid = do
  cfg <- ask
  pipe <- dbPipe cfg
  user <- dbGetById (dbName cfg) userColl uid pipe
  return (mkGetSingleLink uid, user)

-- |
-- Validate a record and run an action on it if valid.
-- Otherwise, throw an error
validateAndRun :: MonadError ServantErr m => Record -> (Record -> m a) -> m a
validateAndRun record f = withValidation (f record) (validateUser record)
  where
    withValidation _ err@(ValidationErrors _) = throw400 (B.pack $ show err)
    withValidation r _ = r

-- |
-- Create multiple users
createMultiple :: [Record]
               -> Api (Headers '[Header "Link" String] [ApiItem Record])
createMultiple inputs = return $ addHeader "user links" (mkResult <$> inputs)
  where
    mkResult = Succ

-- |
-- Replace a single user
replaceSingle :: Text -> Record -> Api Record
replaceSingle uid user =
  if uid == "123"
    then return (user & "_id" .=~ ("584e58195984185eb8000005" :: Text))
    else throwError $
         err404
         { errBody = "A user matching the input id was not found"
         }

-- |
-- Update (replace) multiple users
replaceMultiple :: [Record] -> Api [ApiItem Record]
replaceMultiple users = return (mkResult <$> users)
  where
    mkResult = Succ

-- |
-- Update (modify) a single user
modifySingle :: Text -> Record -> Api Record
modifySingle uid user =
  if uid == "123"
    then return u1
    else throwError $
         err404
         { errBody = "A user matching the input id was not found"
         }

-- |
-- Update (modify) multiple users
modifyMultiple :: [Record] -> Api [ApiItem Record]
modifyMultiple = undefined

-- |
-- Handle a head request for a single user endpoint
headSingle
  :: Text
  -> Api (Headers '[Header "ETag" String, Header "Last-Modified" String] NoContent)
headSingle = undefined

-- |
-- Handle a head request for a multiple users endpoint
headMultiple :: Api (Headers '[Header "ETag" String] NoContent)
headMultiple = undefined

-- |
-- Handle an options request for a single user endpoint
optionsSingle :: Text -> Api (Headers '[Header "Allow" String] NoContent)
optionsSingle = undefined

-- |
-- Handle an options request for a multiple user endpoint
optionsMultiple :: Api (Headers '[Header "Allow" String] NoContent)
optionsMultiple = undefined

-- |
-- Convert a MongoDB @Failure@ into a @ServantErr@
-- TODO parse the MongoDB error object
-- https://docs.mongodb.com/manual/reference/command/getLastError/
throwFailed :: MonadError ServantErr m => Failure -> m a
throwFailed (WriteFailure _ msg) = throw400 (B.pack msg)
throwFailed err = throw500 (B.pack $ show err)

throw400 :: MonadError ServantErr m => B.ByteString -> m a
throw400 = throwErr err400

throw404 :: MonadError ServantErr m => B.ByteString -> m a
throw404 = throwErr err404

throw500 :: MonadError ServantErr m => B.ByteString -> m a
throw500 = throwErr err500

throwErr :: MonadError ServantErr m => ServantErr -> B.ByteString -> m a
throwErr err msg =
  throwError
    err
    { errBody = msg
    }
