{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- ^
-- Handlers for the subscription endpoints
module Handlers.Xandar.Subscriptions where

import Api.Xandar
import Data.Text (Text)
import qualified Handlers.Xandar.Common as C
import Persistence.Xandar.Common (subscriptionDefinition)
import Servant
import Types.Common

-- ^
-- Get a single record by id
getSingle :: Maybe Text -> Text -> Api (Headers '[Header "ETag" String] Record)
getSingle = C.getSingle subscriptionDefinition

-- ^
-- Get multiple records
getMultiple :: ServerT GetMultiple Api
getMultiple = C.getMultiple mkSubscriptionGetMultipleLink subscriptionDefinition

-- ^
-- Delete a single record
deleteSingle :: Text -> Api NoContent
deleteSingle = C.deleteSingle subscriptionDefinition

-- ^
-- Create one or more records
createSingleOrMultiple
  :: ApiData Record
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingleOrMultiple (Single r) = createSingle r
createSingleOrMultiple (Multiple rs) = createMultiple rs

-- ^
-- Create a single record
createSingle
  :: Record
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingle = C.createSingle subscriptionDefinition mkSubscriptionGetSingleLink

-- ^
-- Create multiple records
createMultiple
  :: [Record]
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createMultiple = C.createMultiple subscriptionDefinition mkSubscriptionGetSingleLink

-- ^
-- Update (replace) a single record
replaceSingle :: Text -> Record -> Api Record
replaceSingle = C.replaceSingle subscriptionDefinition

-- ^
-- Update (modify) a single record
modifySingle :: Text -> Record -> Api Record
modifySingle = C.modifySingle subscriptionDefinition

-- ^
-- Update (replace) multiple records
replaceMultiple :: [Record] -> Api [ApiResult]
replaceMultiple = C.replaceMultiple subscriptionDefinition

-- ^
-- Update (modify) multiple records
modifyMultiple :: [Record] -> Api [ApiResult]
modifyMultiple = C.modifyMultiple subscriptionDefinition

-- ^
-- Handle an options request for a single record endpoint
optionsSingle :: Text
              -> Api (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
optionsSingle = C.optionsSingle

-- ^
-- Handle an options request for a multiple record endpoint
optionsMultiple :: Api (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
optionsMultiple = C.optionsMultiple
