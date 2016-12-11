{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for Xandar endpoints
module Handlers.Users.Xandar where

import Api.Xandar
import Data.Text (Text)
import GHC.TypeLits
import Servant
import Servant.API
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
  return $ addHeader "pagination links" (addHeader "10" [u1, u2])

-- |
-- Get a single user by id
getSingle :: Text -> Handler User
getSingle uid =
  if uid == "123"
    then return u1
    else throwError $
         err404 {errBody = "A user matching the input id was not found"}

-- |
-- Delete a single user
deleteSingle :: Text -> Handler NoContent
deleteSingle uid = return NoContent

-- |
-- Create a single user
createSingle :: User -> Handler (Headers '[Header "Location" String] User)
createSingle input =
  return $ addHeader "/users/123" (input {_userId = Just "123"})

-- |
-- Create multiple users
createMultiple :: [User]
               -> Handler (Headers '[Header "Link" String] [ModelOrError User])
createMultiple users = return $ addHeader "user links" (mkResult <$> users)
  where mkResult user = Succ (user {_userId = Just "123"})

-- |
-- Replace a single user
replaceSingle :: Text -> User -> Handler User
replaceSingle uid user =
  if uid == "123"
    then return $ u1 {_userEmail = _userEmail user}
    else throwError $ err404 {errBody = "A user matching the input id was not found"}

-- |
-- Update (replace) multiple users
replaceMultiple :: [User] -> Handler [ModelOrError User]
replaceMultiple users = return (mkResult <$> users)
  where mkResult user = Succ (user {_userId = Just "123"})

-- |
-- Update (modify) a single user
modifySingle :: Text -> User -> Handler User
modifySingle uid user =
  if uid == "123"
    then return $ u1 {_userEmail = _userEmail user}
    else throwError $ err404 {errBody = "A user matching the input id was not found"}

-- |
-- Update (modify) multiple users
modifyMultiple :: [User] -> Handler [ModelOrError User]
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
