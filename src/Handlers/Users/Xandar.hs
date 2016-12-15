{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for Xandar endpoints
module Handlers.Users.Xandar where

import Api.Xandar
import Data.Text (Text)
import GHC.TypeLits
import Persistence.Users.Xandar
import Servant
import Servant.API
import Types.Common
import Types.Xandar

-- |
-- Server definition for the Xandar api
server :: Server XandarApi
server =
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

app :: Application
app = serve (Proxy :: Proxy XandarApi) server

-- |
-- Get multiple users
getMultiple :: Server GetMultiple
getMultiple fields query sort start limit =
  return $ addHeader "pagination links" (addHeader "10" [u4, u4])

-- |
-- Get a single user by id
getSingle :: Text -> Handler Record
getSingle uid =
  if uid == "123"
    then return u4
    else throwError $
         err404 {errBody = "A user matching the input id was not found"}

-- |
-- Delete a single user
deleteSingle :: Text -> Handler NoContent
deleteSingle uid = return NoContent

-- |
-- Create a single user
createSingle :: Record -> Handler (Headers '[Header "Location" String] Record)
createSingle input =
  return $ addHeader "/users/123" input

-- |
-- Create multiple users
createMultiple :: [Record]
               -> Handler (Headers '[Header "Link" String] [ApiItem Record])
createMultiple users = return $ addHeader "user links" (mkResult <$> users)
  where mkResult = Succ

-- |
-- Replace a single user
replaceSingle :: Text -> Record -> Handler Record
replaceSingle uid user =
  if uid == "123"
    then return u4
    else throwError $ err404 {errBody = "A user matching the input id was not found"}

-- |
-- Update (replace) multiple users
replaceMultiple :: [Record] -> Handler [ApiItem Record]
replaceMultiple users = return (mkResult <$> users)
  where mkResult = Succ

-- |
-- Update (modify) a single user
modifySingle :: Text -> Record -> Handler Record
modifySingle uid user =
  if uid == "123"
    then return u4
    else throwError $ err404 {errBody = "A user matching the input id was not found"}

-- |
-- Update (modify) multiple users
modifyMultiple :: [Record] -> Handler [ApiItem Record]
modifyMultiple = undefined

-- |
-- Handle a head request for a single user endpoint
headSingle :: Text -> Handler (Headers '[Header "ETag" String, Header "Last-Modified" String] NoContent)
headSingle = undefined

-- |
-- Handle a head request for a multiple users endpoint
headMultiple :: Handler (Headers '[Header "ETag" String] NoContent)
headMultiple = undefined

-- |
-- Handle an options request for a single user endpoint
optionsSingle :: Text -> Handler (Headers '[Header "Allow" String] NoContent)
optionsSingle = undefined

-- |
-- Handle an options request for a multiple user endpoint
optionsMultiple :: Handler (Headers '[Header "Allow" String] NoContent)
optionsMultiple = undefined
