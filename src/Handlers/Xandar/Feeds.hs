-- |
-- Handlers for feeds endpoints
module Handlers.Xandar.Feeds where

import Api.Xandar
import Handlers.Xandar.Common
import Persistence.Xandar.Feeds
       (feedDefinition, feedIndices, feedColl)
import Servant
import Types.Common

-- |
-- Create an application for providing the feed functionality
app :: ApiConfig -> Application
app = app' apiFeedProxy handlers
  where
    handlers :: ServerT XandarFeedApi Api
    handlers =
      getMultiple feedColl :<|> getSingle feedColl :<|> deleteSingle feedColl :<|>
      createSingleOrMultiple feedDefinition feedColl :<|>
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
appInit = addDbIndices feedIndices