{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- ^
-- Handlers for the subscription endpoints
module Handlers.Xandar.Subscriptions where

import Api.Xandar
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text)
import qualified Handlers.Xandar.Common as C
import Persistence.Common
import Persistence.Facade (dbPipe)
import Persistence.Xandar.Common (subscriptionDefinition)
import Persistence.Xandar.Subscriptions
import Servant
import Types.Common

-- ^
-- Get a single record by id
getSingle :: Maybe Text -> Text -> Api (Headers '[ Header "ETag" String] Record)
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
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingleOrMultiple (Single r) = createSingle r
createSingleOrMultiple (Multiple rs) = createMultiple rs

-- ^
-- Create a single record
createSingle
  :: Record
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingle input = C.mkApiResponse respond (createSingle' input)
  where
    getLink = mkSubscriptionGetSingleLink
    respond = apiItem C.throwApiError (C.mkCreateSingleResult getLink)

-- ^
-- Create multiple records
createMultiple
  :: [Record]
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createMultiple input =
  C.mkApiResponse (C.mkCreateMultipleResult getLink) (createMultiple' input)
  where
    getLink = mkSubscriptionGetSingleLink

createSingle'
  :: (MonadReader ApiConfig m, MonadIO m)
  => Record -> ExceptT ApiError m ApiResult
createSingle' input = head . apiItems <$> createMultiple' [input]

createMultiple'
  :: (MonadIO m, MonadReader ApiConfig m)
  => [Record] -> ExceptT ApiError m ApiResults
createMultiple' input = do
  conf <- ask
  pipe <- dbPipe conf
  ExceptT $
    liftIO $
    runExceptT $
    insertSubscriptions
      input
      (C.dbName conf, pipe)
      (esServer conf, C.esIndex conf)

updateSingle
  :: (MonadIO m, MonadReader ApiConfig m, MonadError ServantErr m)
  => Bool -> RecordId -> Record -> m Record
updateSingle replace rid input = C.mkApiResponse respond update
  where
    respond = apiItem C.throwApiError return
    update = updateSingle' replace rid input

updateMultiple
  :: (MonadReader ApiConfig m, MonadIO m, MonadError ServantErr m)
  => Bool -> [Record] -> m ApiResults
updateMultiple replace input =
  C.mkApiResponse return (updateMultiple' replace input)

updateSingle'
  :: (MonadReader ApiConfig m, MonadIO m)
  => Bool -> RecordId -> Record -> ExceptT ApiError m ApiResult
updateSingle' replace rid input =
  head . apiItems <$> updateMultiple' replace [setIdValue rid input]

updateMultiple'
  :: (MonadIO m, MonadReader ApiConfig m)
  => Bool -> [Record] -> ExceptT ApiError m ApiResults
updateMultiple' replace input = do
  conf <- ask
  pipe <- dbPipe conf
  ExceptT $
    liftIO $
    runExceptT $
    updateSubscriptions
      replace
      input
      (C.dbName conf, pipe)
      (esServer conf, C.esIndex conf)

-- ^
-- Update (replace) a single record
replaceSingle :: Text -> Record -> Api Record
replaceSingle = updateSingle True

-- ^
-- Update (modify) a single record
modifySingle :: Text -> Record -> Api Record
modifySingle = updateSingle False

-- ^
-- Update (replace) multiple records
replaceMultiple :: [Record] -> Api ApiResults
replaceMultiple = updateMultiple True

-- ^
-- Update (modify) multiple records
modifyMultiple :: [Record] -> Api ApiResults
modifyMultiple = updateMultiple False

-- ^
-- Handle an options request for a single record endpoint
optionsSingle :: Text
              -> Api (Headers '[ Header "Access-Control-Allow-Methods" String] NoContent)
optionsSingle = C.optionsSingle

-- ^
-- Handle an options request for a multiple record endpoint
optionsMultiple :: Api (Headers '[ Header "Access-Control-Allow-Methods" String] NoContent)
optionsMultiple = C.optionsMultiple
