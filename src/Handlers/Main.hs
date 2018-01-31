-- ^
-- All handlers
module Handlers.Main where

import Api.Main
import Handlers.Common
import qualified Handlers.Lono as LO
import qualified Handlers.Xandar as XA
import Servant
import Servant.Utils.Enter (Entered)
import Types.Common

app :: ApiConfig -> Application
app = app' apiProxy handlers

handlers :: Entered Handler Api (ServerT MainApi Handler)
handlers = XA.handlers :<|> LO.handlers