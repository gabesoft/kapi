-- ^
-- Handlers for Xandar endpoints
module Handlers.Xandar.Xandar where

import Api.Xandar
import Handlers.Xandar.Common
import qualified Handlers.Xandar.Feeds as F
import qualified Handlers.Xandar.Subscriptions as S
import qualified Handlers.Xandar.UserPosts as U
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
    feedHandlers =
      getMultiple mkFeedGetMultipleLink feedDefinition :<|>
      getSingle feedDefinition :<|>
      F.deleteSingle :<|>
      createSingleOrMultiple feedDefinition mkFeedGetSingleLink :<|>
      replaceSingle feedDefinition :<|>
      replaceMultiple feedDefinition :<|>
      modifySingle feedDefinition :<|>
      modifyMultiple feedDefinition :<|>
      optionsSingle :<|>
      optionsMultiple
    userPostHandlers =
      U.getMultiple mkUserPostGetMultipleLink :<|>
      U.getSingle :<|>
      U.deleteSingle :<|>
      U.createSingleOrMultiple mkUserPostGetSingleLink :<|>
      U.replaceSingle :<|>
      U.replaceMultiple :<|>
      U.modifySingle :<|>
      U.modifyMultiple :<|>
      U.optionsSingle :<|>
      U.optionsMultiple
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
      optionsSingle :<|>
      optionsMultiple

-- ^
-- Perform any initialization to be done on server start
appInit :: ApiConfig -> IO ()
appInit =
  addDbIndices userIndices >>
  addDbIndices feedIndices >>
  addDbIndices postIndices >>
  addDbIndices subscriptionIndices >>
  addDbIndices postQueryIndices >>
  addDbIndices tagsIndices