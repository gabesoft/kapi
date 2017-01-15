-- |
-- Handlers for Xandar endpoints
module Handlers.Xandar.Xandar where

import Api.Xandar
import Handlers.Xandar.Common
import Persistence.Xandar.Feeds (feedDefinition, feedIndices, feedColl)
import Persistence.Xandar.Users (userDefinition, userIndices, userColl)
import Servant
import Types.Common

-- |
-- Create an application for providing the user functionality
app :: ApiConfig -> Application
app = app' apiProxy (userHandlers :<|> feedHandlers)
  where
    userHandlers :: ServerT XandarUserApi Api
    userHandlers =
      getMultiple mkUserGetMultipleLink userColl :<|>
      getSingle userColl :<|>
      deleteSingle userColl :<|>
      createSingleOrMultiple userDefinition userColl mkUserGetSingleLink :<|>
      replaceSingle userDefinition userColl :<|>
      replaceMultiple userDefinition userColl :<|>
      modifySingle userDefinition userColl :<|>
      modifyMultiple userDefinition userColl :<|>
      headSingle userColl :<|>
      headMultiple userColl :<|>
      optionsSingle :<|>
      optionsMultiple
    feedHandlers :: ServerT XandarFeedApi Api
    feedHandlers =
      getMultiple mkFeedGetMultipleLink feedColl :<|>
      getSingle feedColl :<|>
      deleteSingle feedColl :<|>
      createSingleOrMultiple feedDefinition feedColl mkFeedGetSingleLink :<|>
      replaceSingle feedDefinition feedColl :<|>
      replaceMultiple feedDefinition feedColl :<|>
      modifySingle feedDefinition feedColl :<|>
      modifyMultiple feedDefinition feedColl :<|>
      headSingle feedColl :<|>
      headMultiple feedColl :<|>
      optionsSingle :<|>
      optionsMultiple

-- |
-- Perform any initialization to be done on server start
appInit :: ApiConfig -> IO ()
appInit = addDbIndices userIndices >> addDbIndices feedIndices
