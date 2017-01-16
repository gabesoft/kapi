-- |
-- Handlers for Xandar endpoints
module Handlers.Xandar.Xandar where

import Api.Xandar
import Handlers.Xandar.Common
import Persistence.Xandar.Feeds (feedDefinition, feedIndices, feedColl)
import Persistence.Xandar.Posts (postDefinition, postIndices, postColl)
import Persistence.Xandar.Users (userDefinition, userIndices, userColl)
import Servant
import Types.Common

-- |
-- Create an application for providing the user functionality
app :: ApiConfig -> Application
app = app' apiProxy (userHandlers :<|> feedHandlers :<|> postHandlers)
  where
    userHandlers :: ServerT XandarUserApi Api
    userHandlers =
      handlers userDefinition userColl mkUserGetSingleLink mkUserGetMultipleLink
    feedHandlers :: ServerT XandarFeedApi Api
    feedHandlers =
      handlers feedDefinition feedColl mkFeedGetSingleLink mkFeedGetMultipleLink
    postHandlers :: ServerT XandarPostApi Api
    postHandlers =
      handlers postDefinition postColl mkPostGetSingleLink mkPostGetMultipleLink
    handlers def coll mkGetSingleLink mkGetMultipleLink =
        getMultiple mkGetMultipleLink coll :<|>
        getSingle coll :<|>
        deleteSingle coll :<|>
        createSingleOrMultiple def coll mkGetSingleLink :<|>
        replaceSingle def coll :<|>
        replaceMultiple def coll :<|>
        modifySingle def coll :<|>
        modifyMultiple def coll :<|>
        headSingle coll :<|>
        headMultiple coll :<|>
        optionsSingle :<|>
        optionsMultiple

-- |
-- Perform any initialization to be done on server start
appInit :: ApiConfig -> IO ()
appInit = addDbIndices userIndices >> addDbIndices feedIndices >> addDbIndices postIndices
