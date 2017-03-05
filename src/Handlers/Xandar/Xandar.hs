-- ^
-- Handlers for Xandar endpoints
module Handlers.Xandar.Xandar where

import Api.Xandar
import Handlers.Xandar.Common
import qualified Handlers.Xandar.Subscriptions as S
import qualified Handlers.Xandar.UserPosts as UP
import Persistence.Xandar.Common
import Servant
import Types.Common

-- ^
-- Create an application for providing the user functionality
app :: ApiConfig -> Application
app =
  app' apiProxy (userHandlers :<|>
                 feedHandlers :<|>
                 postHandlers :<|>
                 subscriptionHandlers :<|>
                 userPostHandlers :<|>
                 postQueryHandlers :<|>
                 tagsHandlers
                )
  where
    userHandlers :: ServerT XandarUserApi Api
    userHandlers = handlers userDefinition mkUserGetSingleLink mkUserGetMultipleLink
    feedHandlers :: ServerT XandarFeedApi Api
    feedHandlers = handlers feedDefinition mkFeedGetSingleLink mkFeedGetMultipleLink
    postHandlers :: ServerT XandarPostApi Api
    postHandlers = handlers postDefinition mkPostGetSingleLink mkPostGetMultipleLink
    postQueryHandlers :: ServerT XandarPostQueryApi Api
    postQueryHandlers = handlers postQueryDefinition mkPostQueryGetSingleLink mkPostQueryGetMultipleLink
    tagsHandlers :: ServerT XandarTagsApi Api
    tagsHandlers = handlers tagsDefinition mkTagsGetSingleLink mkTagsGetMultipleLink
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
      UP.getMultiple mkUserPostGetMultipleLink :<|>
      UP.getSingle :<|>
      UP.deleteSingle :<|>
      UP.createSingleOrMultiple mkUserPostGetSingleLink :<|>
      UP.replaceSingle :<|>
      UP.replaceMultiple :<|>
      UP.modifySingle :<|>
      UP.modifyMultiple :<|>
      UP.optionsSingle :<|>
      UP.optionsMultiple
    subscriptionHandlers :: ServerT XandarSubscriptionApi Api
    subscriptionHandlers =
      S.getMultiple :<|>
      S.getSingle :<|>
      S.deleteSingle :<|>
      S.createSingleOrMultiple :<|>
      S.replaceSingle :<|>
      S.replaceMultiple :<|>
      S.modifySingle :<|>
      S.modifyMultiple :<|>
      S.optionsSingle :<|>
      S.optionsMultiple

-- ^
-- Perform any initialization to be done on server start
appInit :: ApiConfig -> IO ()
appInit =
  addDbIndices userIndices >>
  addDbIndices feedIndices >>
  addDbIndices postIndices >>
  addDbIndices subscriptionIndices >>
  addDbIndices postQueryIndices