-- ^
-- Handlers for Xandar endpoints
module Handlers.Xandar where

import Api.Xandar
import Handlers.Common
import qualified Handlers.RssReaders.Feeds as F
import qualified Handlers.RssReaders.Subscriptions as S
import qualified Handlers.RssReaders.UserPosts as U
import Persistence.RssReaders.Common
import Persistence.Xandar.Common
import Servant
import Types.Common

-- ^
-- Create an application for providing the user functionality
app :: ApiConfig -> Application
app = app' apiProxy handlers

handlers :: ServerT XandarApi Api
handlers =
    userHandlers :<|>
    feedHandlers :<|>
    postHandlers :<|>
    subscriptionHandlers :<|>
    userPostHandlers :<|>
    postQueryHandlers :<|>
    tagsHandlers
  where
    userHandlers :: ServerT XandarUserApi Api
    userHandlers = mkHandlers userDefinition mkUserGetSingleLink mkUserGetMultipleLink
    postHandlers :: ServerT XandarPostApi Api
    postHandlers = mkHandlers postDefinition mkPostGetSingleLink mkPostGetMultipleLink
    postQueryHandlers :: ServerT XandarPostQueryApi Api
    postQueryHandlers = mkHandlers postQueryDefinition mkPostQueryGetSingleLink mkPostQueryGetMultipleLink
    tagsHandlers :: ServerT XandarTagsApi Api
    tagsHandlers = mkHandlers tagsDefinition mkTagsGetSingleLink mkTagsGetMultipleLink
    mkHandlers def mkGetSingleLink mkGetMultipleLink =
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
      S.getMultiple mkSubscriptionGetMultipleLink :<|>
      S.getSingle :<|>
      S.deleteSingle :<|>
      S.createSingleOrMultiple mkSubscriptionGetSingleLink :<|>
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