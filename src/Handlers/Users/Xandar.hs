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
  getMultiple :<|> getSingle :<|> deleteSingle :<|> createSingle :<|>
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
-- Handler for getting multiple users
getMultiple :: Server GetMultiple
getMultiple fields query sort start limit =
  return $ addHeader "pagination links" (addHeader "10" [u1, u2])

-- |
-- Handler for returning a single user by id
getSingle :: Text -> Handler User
getSingle uid =
  if uid == "1"
    then return u1
    else throwError $
         err404 {errBody = "A user matching the input id was not found"}

-- |
-- Handler for deleting a single user
deleteSingle :: Text -> Handler NoContent
deleteSingle uid = return NoContent

-- |
-- Handler for creating a single user
createSingle = undefined

createMultiple = undefined

replaceSingle = undefined

replaceMultiple = undefined

modifySingle = undefined

modifyMultiple = undefined

headSingle = undefined

headMultiple = undefined

optionsSingle = undefined

optionsMultiple = undefined
