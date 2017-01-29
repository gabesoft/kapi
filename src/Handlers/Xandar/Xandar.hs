-- |
-- Handlers for Xandar endpoints
module Handlers.Xandar.Xandar where

import Api.Xandar
import Handlers.Xandar.Common
import qualified Handlers.Xandar.UserPosts as UP
import Persistence.Xandar.FeedSubscriptions
       (feedSubscriptionDefinition, feedSubscriptionIndices)
import Persistence.Xandar.Feeds (feedDefinition, feedIndices)
import Persistence.Xandar.Posts (postDefinition, postIndices)
import Persistence.Xandar.Users (userDefinition, userIndices)
import Servant
import Types.Common

-- |
-- Create an application for providing the user functionality
app :: ApiConfig -> Application
app =
  app'
    apiProxy
    (userHandlers :<|> feedHandlers :<|> postHandlers :<|> subscriptionHandlers :<|> userPostHandlers)
  where
    userHandlers :: ServerT XandarUserApi Api
    userHandlers =
      handlers userDefinition mkUserGetSingleLink mkUserGetMultipleLink
    feedHandlers :: ServerT XandarFeedApi Api
    feedHandlers =
      handlers feedDefinition mkFeedGetSingleLink mkFeedGetMultipleLink
    postHandlers :: ServerT XandarPostApi Api
    postHandlers =
      handlers postDefinition mkPostGetSingleLink mkPostGetMultipleLink
    subscriptionHandlers :: ServerT XandarSubscriptionApi Api
    subscriptionHandlers =
      handlers
        feedSubscriptionDefinition
        mkSubscriptionGetSingleLink
        mkSubscriptionGetMultipleLink
    handlers def mkGetSingleLink mkGetMultipleLink =
      getMultiple mkGetMultipleLink def :<|>
      getSingle def :<|>
      deleteSingle def :<|>
      createSingleOrMultiple def mkGetSingleLink :<|>
      replaceSingle def :<|>
      replaceMultiple def :<|>
      modifySingle def :<|>
      modifyMultiple def :<|>
      optionsSingle :<|>
      optionsMultiple
    userPostHandlers =
      UP.getMultiple :<|>
      UP.getSingle :<|>
      UP.deleteSingle :<|>
      UP.createSingleOrMultiple :<|>
      UP.replaceSingle :<|>
      UP.replaceMultiple :<|>
      UP.modifySingle :<|>
      UP.modifyMultiple :<|>
      UP.optionsSingle :<|>
      UP.optionsMultiple

-- |
-- Perform any initialization to be done on server start
appInit :: ApiConfig -> IO ()
appInit =
  addDbIndices userIndices >> addDbIndices feedIndices >>
  addDbIndices postIndices >>
  addDbIndices feedSubscriptionIndices
