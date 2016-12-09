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

-- getMultiple
--   :: (KnownSymbol a, KnownSymbol b)
--   => String
--   -> String
--   -> String
--   -> String
--   -> String
--   -> String
--   -> Handler (Headers '[Header a String, Header b String] [User])
getMultiple fields query sort start limit =
  return $ addHeader "abcd" (addHeader "10" [u1, u2])

getSingle :: Text -> Handler User
getSingle uid =
  if uid == "1"
    then return u1
    else throwError $
         err404 {errBody = "A user matching the input id was not found"}

deleteSingle = undefined

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
