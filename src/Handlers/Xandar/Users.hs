{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Handlers for Xandar endpoints
module Handlers.Xandar.Users where

import Api.Xandar
import Handlers.Xandar.Common
import Persistence.Xandar.Users (userDefinition, userIndices, userColl)
import Servant
import Types.Common

-- |
-- Create an application for providing the user functionality
app :: ApiConfig -> Application
app = app' apiUserProxy handlers
  where
    handlers :: ServerT XandarUserApi Api
    handlers =
      getMultiple userColl :<|>
      getSingle userColl :<|>
      deleteSingle userColl :<|>
      createSingleOrMultiple userDefinition userColl :<|>
      replaceSingle userDefinition userColl :<|>
      replaceMultiple userDefinition userColl :<|>
      modifySingle userDefinition userColl :<|>
      modifyMultiple userDefinition userColl :<|>
      headSingle userColl :<|>
      headMultiple userColl :<|>
      optionsSingle :<|>
      optionsMultiple

-- |
-- Perform any initialization to be done on server start
appInit :: ApiConfig -> IO ()
appInit = addDbIndices userIndices
