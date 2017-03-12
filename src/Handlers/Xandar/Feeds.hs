{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Handlers for the feeds endpoints
module Handlers.Xandar.Feeds where

import Control.Monad.Except
import Data.Monoid ((<>))
import Data.Text (Text)
import Handlers.Xandar.Common (runSingle)
import Persistence.Facade
import Persistence.MongoDB (queryToDoc)
import Persistence.Xandar.Common (postDefinition, feedDefinition)
import Servant
import Types.Common

-- ^
-- Delete a single feed and all associated posts
deleteSingle :: Text -> Api NoContent
deleteSingle uid
 = do
  _ <- runExceptT (dbDeleteByQuery postDefinition query)
  runSingle (dbDelete feedDefinition uid) (const $ return NoContent)
  where
    (Right query) = queryToDoc postDefinition ("feedId eq " <> uid)
