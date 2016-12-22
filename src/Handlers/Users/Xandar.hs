{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for Xandar endpoints
module Handlers.Users.Xandar where

import Data.ByteString.Lazy.Char8
import Api.Xandar
import Control.Monad.Reader
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Persistence.Users.Xandar
import Servant
import Types.Common

handlers :: ServerT XandarApi (ReaderT ApiConfig Handler)
handlers
 =
  getMultipleT :<|>
  getSingleT :<|>
  deleteSingleT :<|>
  createSingleT :<|>
  createMultipleT :<|>
  replaceSingleT :<|>
  replaceMultipleT :<|>
  modifySingleT :<|>
  modifyMultipleT :<|>
  headSingleT :<|>
  headMultipleT :<|>
  optionsSingleT :<|>
  optionsMultipleT

app :: ApiConfig -> Application
app config = serve (Proxy :: Proxy XandarApi) (server config)

server :: ApiConfig -> Server XandarApi
server config = enter (readerToHandler config) handlers

readerToHandler :: ApiConfig -> ReaderT ApiConfig Handler :~> Handler
readerToHandler config = Nat $ flip runReaderT config

-- |
-- Get multiple users
-- getMultiple :: Server GetMultiple
-- getMultiple fields query sort start limit =
--   return $ addHeader "pagination links" (addHeader "10" [u1, u2])
getMultipleT = undefined

-- |
-- Get a single user by id
getSingleT :: Text -> ReaderT ApiConfig Handler Record
getSingleT uid =
  if uid == "123"
    then return u1
    else throwError $
         err404
         { errBody =
           fromStrict . encodeUtf8 $
           T.concat ["A user with id ", uid, " was not found"]
         }

-- |
-- Delete a single user
deleteSingleT = undefined
deleteSingle :: Text -> Handler NoContent
deleteSingle uid = return NoContent

-- |
-- Create a single user
createSingleT = undefined
createSingle :: Record -> Handler (Headers '[Header "Location" String] Record)
createSingle input = return $ addHeader "/users/123" input

-- |
-- Create multiple users
createMultipleT = undefined
createMultiple :: [Record]
               -> Handler (Headers '[Header "Link" String] [ApiItem Record])
createMultiple users = return $ addHeader "user links" (mkResult <$> users)
  where
    mkResult = Succ

-- |
-- Replace a single user
replaceSingleT = undefined
replaceSingle :: Text -> Record -> Handler Record
replaceSingle uid user =
  if uid == "123"
    then return (user & "_id" .=~ ("584e58195984185eb8000005" :: Text))
    else throwError $
         err404
         { errBody = "A user matching the input id was not found"
         }

-- |
-- Update (replace) multiple users
replaceMultipleT = undefined
replaceMultiple :: [Record] -> Handler [ApiItem Record]
replaceMultiple users = return (mkResult <$> users)
  where
    mkResult = Succ

-- |
-- Update (modify) a single user
modifySingleT = undefined
modifySingle :: Text -> Record -> Handler Record
modifySingle uid user =
  if uid == "123"
    then return u1
    else throwError $
         err404
         { errBody = "A user matching the input id was not found"
         }

-- |
-- Update (modify) multiple users
modifyMultipleT = undefined
modifyMultiple :: [Record] -> Handler [ApiItem Record]
modifyMultiple = undefined

-- |
-- Handle a head request for a single user endpoint
headSingleT = undefined
headSingle
  :: Text
  -> Handler (Headers '[Header "ETag" String, Header "Last-Modified" String] NoContent)
headSingle = undefined

-- |
-- Handle a head request for a multiple users endpoint
headMultipleT = undefined
headMultiple :: Handler (Headers '[Header "ETag" String] NoContent)
headMultiple = undefined

-- |
-- Handle an options request for a single user endpoint
optionsSingleT = undefined
optionsSingle :: Text -> Handler (Headers '[Header "Allow" String] NoContent)
optionsSingle = undefined

-- |
-- Handle an options request for a multiple user endpoint
optionsMultipleT = undefined
optionsMultiple :: Handler (Headers '[Header "Allow" String] NoContent)
optionsMultiple = undefined
