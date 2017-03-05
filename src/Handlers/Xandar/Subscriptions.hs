{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- ^
-- Handlers for the subscription endpoints
module Handlers.Xandar.Subscriptions where

import Api.Common (GetMultiple)
import Api.Xandar
import Data.Text (Text)
import Handlers.Xandar.Common
       (runSingle, runMulti, mkGetSingleResult, mkGetMultipleResult,
        getMultiple', mkCreateSingleResult, mkCreateMultipleResult,
        updateSingle)
import qualified Handlers.Xandar.Common as C
import Persistence.Xandar.Common (subscriptionDefinition)
import Persistence.Xandar.Subscriptions
import Servant
import Types.Common

-- ^
-- Get a single record by id
getSingle :: Maybe Text -> Text -> Api (Headers '[ Header "ETag" String] Record)
getSingle etag sid = runSingle (getSubscription sid) return >>= mkGetSingleResult etag

-- ^
-- Get multiple records
getMultiple :: ServerT GetMultiple Api
getMultiple include query sort page perPage =
  runSingle getRecords (return . mkResult)
  where
    getLink = mkSubscriptionGetMultipleLink
    def = subscriptionDefinition
    getRecords = getMultiple' getSubscriptions def include query sort page perPage
    mkResult = mkGetMultipleResult getLink include query sort perPage

-- ^
-- Delete a single record
deleteSingle :: Text -> Api NoContent
deleteSingle uid = runSingle (deleteSubscription uid) (const $ return NoContent)

-- ^
-- Create one or more records
createSingleOrMultiple
  :: ApiData Record
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingleOrMultiple (Single r) = createSingle r
createSingleOrMultiple (Multiple rs) = createMultiple rs

-- ^
-- Create a single record
createSingle
  :: Record
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingle input =
  runSingle
    (insertSubscription input)
    (mkCreateSingleResult mkSubscriptionGetSingleLink)

-- ^
-- Create multiple records
createMultiple
  :: [Record]
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createMultiple input =
  runMulti (insertSubscriptions input) >>=
  mkCreateMultipleResult mkSubscriptionGetSingleLink

-- ^
-- Update (replace) a single record
replaceSingle :: Text -> Record -> Api Record
replaceSingle = updateSingle replaceSubscription

-- ^
-- Update (modify) a single record
modifySingle :: Text -> Record -> Api Record
modifySingle = updateSingle modifySubscription

-- ^
-- Update (replace) multiple records
replaceMultiple :: [Record] -> Api [ApiResult]
replaceMultiple = runMulti . replaceSubscriptions

-- ^
-- Update (modify) multiple records
modifyMultiple :: [Record] -> Api [ApiResult]
modifyMultiple = runMulti . modifySubscriptions

-- ^
-- Handle an options request for a single record endpoint
optionsSingle :: Text
              -> Api (Headers '[ Header "Access-Control-Allow-Methods" String] NoContent)
optionsSingle = C.optionsSingle

-- ^
-- Handle an options request for a multiple record endpoint
optionsMultiple :: Api (Headers '[ Header "Access-Control-Allow-Methods" String] NoContent)
optionsMultiple = C.optionsMultiple
