{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- ^
-- Facade that provides consistent access to persistence functionality
module Persistence.Facade
  ( DbAction
  , EsAction
  , RunDb
  , RunEs
  , runDb
  , runEs
  , validate
  , validate'
  , validateId
  , validateId'
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson (encode)
import Data.Bifunctor
import Data.Either
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.Bloodhound.Types (EsError, SearchResult)
import Database.MongoDB (Pipe, Database, Failure)
import Network.HTTP.Types.Status
import Persistence.Common
import Persistence.ElasticSearch
import Persistence.MongoDB
import Types.Common

type DbAction m a = Database -> Pipe -> m (Either Failure a)

type EsAction a = Text -> Text -> IO (Either EsError a)

type RunDb m a b = (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m) =>
                     DbAction m a -> ExceptT ApiError m b

type RunEs m a b = (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m) =>
                     EsAction a -> ExceptT ApiError m b

dbInsertRecords
  :: ( MonadIO m
     , MonadReader ApiConfig (ApiItems2T [ApiError] m)
     , MonadBaseControl IO m
     )
  => AppName -> RecordDefinition -> [Record] -> ApiItems2T [ApiError] m [Record]
dbInsertRecords appName def input = do
  validated <- validateManyT def input
  savedIds <- runDbT appName (save validated)
  saved <- runDbT appName (get savedIds)
  return (fromJust <$> saved)
  where
    save = dbActions dbInsert def
    get = dbActions dbGetById def

dbUpdateRecords
  :: AppName
  -> Bool
  -> RecordDefinition
  -> [Record]
  -> ExceptT ApiError m [ApiResult]
dbUpdateRecords appName replace def input = undefined

-- ^
-- Run multiple MongoDB actions and return all results
dbActions
  :: (Monad m)
  => (RecordDefinition -> b -> Database -> Pipe -> m (Either Failure e))
  -> RecordDefinition
  -> [b]
  -> Database
  -> Pipe
  -> m [Either Failure e]
dbActions singleAction def rs dbName pipe =
  mapM (\r -> singleAction def r dbName pipe) rs

validateManyT :: (Monad m) => RecordDefinition -> [Record] -> ApiItems2T [ApiError] m [Record]
validateManyT def xs =
  ApiItems2T . return . concatItems $
  (vResultToItems . validateRecord def) <$> xs

runDbT
  :: ( MonadIO m
     , MonadReader ApiConfig (ApiItems2T [ApiError] m)
     , MonadBaseControl IO m
     )
  => AppName
  -> (Database -> Pipe -> m [Either Failure a])
  -> ApiItems2T [ApiError] m [a]
runDbT appName action = do
  conf <- ask
  pipe <- dbPipe conf
  results <- lift $ action (confGetDb appName conf) pipe
  let items = first dbToApiError <$> results
  ApiItems2T . return $ ApiItems2 (lefts items) (rights items)

-- ^
-- Run a MongoDB action
runDb :: AppName -> RunDb m a a
runDb appName action = do
  conf <- ask
  pipe <- dbPipe conf
  results <- lift $ action (confGetDb appName conf) pipe
  ExceptT (return $ first dbToApiError results)

-- ^
-- Run an Elastic-Search action
runEs :: AppName -> RunEs m a a
runEs appName action = do
  conf <- ask
  results <- liftIO $ action (esServer conf) (confGetEsIndex appName conf)
  ExceptT (return $ first esToApiError results)

-- ^
-- Run an elastic-search action and extract the results
runEsAndExtract :: AppName -> RunEs m (SearchResult Record) [Record]
runEsAndExtract appName = fmap (extractRecords []) . runEs appName

-- ^
-- Ensure that a record is valid according to its definition
validate :: RecordDefinition -> Record -> Either ApiError Record
validate def = vResultToEither . validateRecord def

validateMany :: RecordDefinition -> [Record] -> ApiResults2
validateMany def xs =
  foldr (<>) mempty $ (vResultToItems . validateRecord def) <$> xs

-- ^
-- Ensure that a record has a valid id
validateId :: Record -> Either ApiError Record
validateId = vResultToEither . validateRecordHasId

-- ^
-- Ensure that a record is valid according to its definition
validate'
  :: Monad m
  => RecordDefinition -> Record -> ExceptT ApiError m Record
validate' def = ExceptT . return . validate def

-- ^
-- Ensure that a record has a valid id
validateId'
  :: Monad m
  => Record -> ExceptT ApiError m Record
validateId' =  ExceptT . return . validateId

-- ^
-- Convert the result of a validation to 'Either'
vResultToEither :: (a, ValidationResult) -> Either ApiError a
vResultToEither (a, ValidationErrors []) = Right a
vResultToEither (_, err) = Left (ApiError status400 (encode err))

vResultToItems :: (Record, ValidationResult) -> ApiResults2
vResultToItems (a, ValidationErrors []) = ApiItems2 [] [a]
vResultToItems (_, err) = ApiItems2 [ApiError status400 (encode err)] []

-- ^
-- Create a MongoDB connection pipe
dbPipe
  :: MonadIO m
  => ApiConfig -> m Pipe
dbPipe conf = liftIO $ mkPipe (mongoHost conf) (mongoPort conf)
