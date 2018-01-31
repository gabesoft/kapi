-- ^
-- Handlers for Lono endpoints
module Handlers.Lono where

import Api.Lono
import Handlers.Common
import qualified Handlers.RssReaders.Feeds as F
import qualified Handlers.RssReaders.Subscriptions as S
import qualified Handlers.RssReaders.UserPosts as U
import Persistence.Lono.Common
import Persistence.RssReaders.Common
import Servant
import Types.Common

-- ^
-- Create an application containing all Lono API endpoints
app :: ApiConfig -> Application
app = app' apiProxy handlers

-- ^
-- Lono API handlers
handlers :: ServerT LonoApi Api
handlers =
    userHandlers :<|>
    feedHandlers :<|>
    postHandlers :<|>
    subscriptionHandlers :<|>
    userPostHandlers :<|>
    postQueryHandlers :<|>
    tagsHandlers
  where
    userHandlers :: ServerT LonoUserApi Api
    userHandlers = mkHandlers userDefinition mkUserGetSingleLink mkUserGetMultipleLink
    postHandlers :: ServerT LonoPostApi Api
    postHandlers = mkHandlers postDefinition mkPostGetSingleLink mkPostGetMultipleLink
    postQueryHandlers :: ServerT LonoPostQueryApi Api
    postQueryHandlers = mkHandlers postQueryDefinition mkPostQueryGetSingleLink mkPostQueryGetMultipleLink
    tagsHandlers :: ServerT LonoTagsApi Api
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
    subscriptionHandlers :: ServerT LonoSubscriptionApi Api
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
