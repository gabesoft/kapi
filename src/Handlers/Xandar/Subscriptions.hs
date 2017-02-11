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
import Persistence.Xandar.Subscriptions
import Servant
import Types.Common

mkGetSingleLink :: Text -> String
mkGetSingleLink = mkSubscriptionGetSingleLink

mkGetMultipleLink :: ApiGetMultipleLink
mkGetMultipleLink = mkSubscriptionGetMultipleLink

getMultiple :: ServerT GetMultiple Api
getMultiple = C.getMultiple mkGetMultipleLink subscriptionDefinition

getSingle :: Maybe Text -> Text -> Api (Headers '[Header "ETag" String] Record)
getSingle = C.getSingle subscriptionDefinition

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
createSingle = C.createSingle subscriptionDefinition mkGetSingleLink

-- ^
-- Create multiple records
createMultiple
  :: [Record]
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createMultiple = C.createMultiple subscriptionDefinition mkGetSingleLink

replaceSingle :: Text -> Record -> Api Record
replaceSingle = C.replaceSingle subscriptionDefinition

replaceMultiple :: [Record] -> Api [ApiResult]
replaceMultiple = C.replaceMultiple subscriptionDefinition

modifySingle :: Text -> Record -> Api Record
modifySingle = C.modifySingle subscriptionDefinition

modifyMultiple :: [Record] -> Api [ApiResult]
modifyMultiple = C.modifyMultiple subscriptionDefinition

optionsSingle :: Text
              -> Api (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
optionsSingle = C.optionsSingle

optionsMultiple :: Api (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
optionsMultiple = C.optionsMultiple
