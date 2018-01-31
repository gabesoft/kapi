{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- ^
-- Handlers for the subscription endpoints
module Handlers.RssReaders.Subscriptions where

import Api.Common (GetMultiple, ApiGetMultipleLink)
import Data.Text (Text)
import Handlers.Common
       (runSingle, runMulti, mkGetSingleResult, mkGetMultipleResult,
        getMultiple', mkCreateSingleResult, mkCreateMultipleResult,
        updateSingle)
import Persistence.RssReaders.Common (subscriptionDefinition)
import Persistence.RssReaders.Subscriptions
import Servant
import Types.Common

-- ^
-- Get a single record by id
getSingle :: Maybe Text -> Text -> Api (Headers '[ Header "ETag" String] Record)
getSingle etag sid = runSingle (getSubscription sid) return >>= mkGetSingleResult etag

-- ^
-- Get multiple records
getMultiple :: ApiGetMultipleLink -> ServerT GetMultiple Api
getMultiple getLink include query sort page perPage =
  runSingle getRecords (return . mkResult)
  where
    def = subscriptionDefinition
    getRecords = getMultiple' getSubscriptions def include query sort page perPage
    mkResult = mkGetMultipleResult getLink include query sort perPage

-- ^
-- Delete a single record
deleteSingle :: Text -> Api NoContent
deleteSingle uid = runSingle (deleteSubscription uid) (const $ return NoContent)

-- ^
-- Create one or more records
createSingleOrMultiple ::
     (Text -> String)
  -> ApiData Record
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingleOrMultiple getLink (Single r) = createSingle getLink r
createSingleOrMultiple getLink (Multiple rs) = createMultiple getLink rs

-- ^
-- Create a single record
createSingle ::
     (Text -> String)
  -> Record
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingle getLink input =
  runSingle
    (insertSubscription input)
    (mkCreateSingleResult getLink)

-- ^
-- Create multiple records
createMultiple ::
     (Text -> String)
  -> [Record]
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createMultiple getLink input =
  runMulti (insertSubscriptions input) >>= mkCreateMultipleResult getLink

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
