{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
-- ^
-- All handlers
module Handlers.Main (app) where

import Api.Main
import Control.Monad.Reader
import qualified Handlers.Lono as LO
import qualified Handlers.Xandar as XA
import Servant
import Servant.Utils.Enter ((:~>)(NT), Enter, Entered, enter)
import Types.Common

-- ^
-- Create the main application containing all API endpoints
app :: ApiConfig -> ApiConfig -> Application
app xaConf loConf = serve mainProxy server
  where
    server :: Server MainApi
    server = server' XA.handlers xaConf :<|> server' LO.handlers loConf
    server' ::
         (Enter (Entered Handler Api ret) Api Handler ret)
      => Entered Handler Api ret
      -> ApiConfig
      -> ret
    server' hs cfg = enter (toHandler cfg) hs
    toHandler :: ApiConfig -> Api :~> Handler
    toHandler conf = NT (`runReaderT` conf)
