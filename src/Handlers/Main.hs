-- ^
-- All handlers
module Handlers.Main where

import Api.Main
import Handlers.Common
import qualified Handlers.Lono as LO
import qualified Handlers.Xandar as XA
import Servant
import Types.Common

app :: ApiConfig -> Application
app = app' mainProxy handlers

handlers :: ServerT MainApi Api
handlers = XA.handlers :<|> LO.handlers

