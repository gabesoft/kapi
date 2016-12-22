{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for Xandar endpoints
module Handlers.Users.Xandar where

import Api.Xandar
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Function ((&))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Database.MongoDB (Pipe)
import Persistence.MongoDB
import Persistence.Users.Xandar (u1, u2)
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

userColl = "users"

dbName = confGetDb appName

dbPipe :: ApiConfig -> Api Pipe
dbPipe cfg = liftIO $ mkPipe (mongoHost cfg) (mongoPort cfg)

app :: ApiConfig -> Application
app config = serve (Proxy :: Proxy XandarApi) (server config)
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
getSingle uid = do
  cfg <- ask
  user <- dbPipe cfg >>= dbGetById (dbName cfg) userColl uid
  maybe (throwError err404) return user

-- |
-- Delete a single user
deleteSingle :: Text -> Api NoContent
deleteSingle uid = do
  cfg <- ask
  getSingle uid
  dbPipe cfg >>= dbDeleteById (dbName cfg) userColl uid
  return NoContent

-- |
-- Create a single user
createSingle :: Record -> Api (Headers '[Header "Location" String] Record)
createSingle input = do
  cfg <- ask
  pipe <- dbPipe cfg
  -- TODO add validation and return 400 on error
  uid <- dbInsert (dbName cfg) userColl input pipe
  user <- dbGetById (dbName cfg) userColl uid pipe
  return $ addHeader ("/users/" ++ T.unpack uid) (fromJust user)

-- |
-- Create multiple users
createMultiple :: [Record]
               -> Api (Headers '[Header "Link" String] [ApiItem Record])
createMultiple users = return $ addHeader "user links" (mkResult <$> users)
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
