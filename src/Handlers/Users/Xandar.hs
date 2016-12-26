{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Handlers for Xandar endpoints
module Handlers.Users.Xandar where

import Data.List (intercalate)
import Api.Xandar
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (encode)
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

dbPipe :: ApiConfig -> Api Pipe
dbPipe cfg = liftIO $ mkPipe (mongoHost cfg) (mongoPort cfg)

validateUser :: Record -> (Record, ValidationResult)
validateUser r = (r, validate userDefinition r)

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
getMultiple fields query sort start limit = do
  -- TODO add sorting, pagination, etc
  cfg <- ask
  users <- dbPipe cfg >>= dbFind (dbName cfg) userColl
  return $ addHeader "pagination links" (addHeader (show $ length users) users)

-- |
-- Get a single user by id
getSingle :: Text -> Api Record
getSingle uid = snd <$> getRecordAndLink uid >>= maybe (throw404 mempty) return

-- |
-- Delete a single user
deleteSingle :: Text -> Api NoContent
deleteSingle uid = do
  _ <- getSingle uid
  cfg <- ask
  dbPipe cfg >>= dbDeleteById (dbName cfg) userColl uid
  return NoContent

-- |
-- Create one or more users
createSingleOrMultiple
  :: ApiData Record
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData (ApiItem Record)))
createSingleOrMultiple (Single r) = createSingle r
createSingleOrMultiple (Multiple rs) = createMultiple rs

-- |
-- Create a single user
createSingle
  :: Record
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData (ApiItem Record)))
createSingle input = validateAndRun input (insertSingle >=> mkResult)
  where
    mkResult (link, Right r) = return $ addHeader link $ noHeader (mkData r)
    mkResult (_, Left err) = throwFailed err
    mkData = Single . Succ

-- |
-- Insert a single valid user
insertSingle :: Record -> Api (String, Either Failure Record)
insertSingle input = do
  cfg <- ask
  pipe <- dbPipe cfg
  result <- dbInsert (dbName cfg) userColl (populateUserDefaults input) pipe
  case result of
    Left err -> return (mempty, Left err)
    Right uid -> second (Right . fromJust) <$> getRecordAndLink uid

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
    withValidation r (_, ValidationErrors []) = r
    withValidation _ (_, err) = throw400 (show err)

-- |
-- Create multiple users
createMultiple :: [Record]
               -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData (ApiItem Record)))
createMultiple inputs = do
  results <- mapM insert (toEither . validateUser <$> inputs)
  let links = mkLink . fst <$> results
  let users = snd <$> results
  return $
    noHeader $
    addHeader (intercalate ", " links) (Multiple $ toApiItem <$> users)
  where
    mkLink path = "<" ++ path ++ ">"
    toEither (r, ValidationErrors []) = Right r
    toEither (_, err) = Left (apiErrorWrap err)
    insert :: Either ApiError Record -> Api (String, Either ApiError Record)
    insert = either (return . (,) mempty . Left) (fmap convert . insertSingle)
    convert :: (String, Either Failure Record)
            -> (String, Either ApiError Record)
    convert = second $ first apiErrorWrap
    toApiItem (Left err) = Fail err
    toApiItem (Right r) = Succ r

-- |
-- Update (replace) a single user
replaceSingle :: Text -> Record -> Api Record
replaceSingle uid input =
  getSingle uid >> validateAndRun user (updateSingle >=> mkResult)
  where
    user = setValue "_id" uid input
    mkResult (Left err) = throwFailed err
    mkResult (Right r) = return r

-- |
-- Update an existing valid user
updateSingle :: Record -> Api (Either Failure Record)
updateSingle input = do
  cfg <- ask
  pipe <- dbPipe cfg
  let uid = fromJust (getValue "_id" input)
  let db = dbName cfg
  result <- dbUpdate db userColl (populateUserDefaults input) pipe
  case result of
    Left err -> return (Left err)
    Right _ -> Right . fromJust <$> dbGetById db userColl uid pipe

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
-- TODO parse the MongoDB error object (the error is in BSON format)
-- https://docs.mongodb.com/manual/reference/command/getLastError/
throwFailed :: MonadError ServantErr m => Failure -> m a
throwFailed (WriteFailure _ msg) = throw400 msg
throwFailed err = throw500 (show err)

throw400 :: MonadError ServantErr m => String -> m a
throw400 = throwErr err400

throw404 :: MonadError ServantErr m => String -> m a
throw404 = throwErr err404

throw500 :: MonadError ServantErr m => String -> m a
throw500 = throwErr err500

throwErr :: MonadError ServantErr m => ServantErr -> String -> m a
throwErr err msg =
  throwError
    err
    { errBody = encode (Fail $ ApiError msg :: ApiItem ())
    }
