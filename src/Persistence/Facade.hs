{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- ^
-- Facade that provides consistent access to persistence functionality
module Persistence.Facade
  ( DbAction
  , EsAction
  , RunDb
  , RunEs
  , dbInsertRecords
  , dbUpdateRecords
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
import Data.Bson (Label)
import Data.Either
import qualified Data.Map.Strict as Map
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

type RunDb m a b = (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m) => DbAction m a -> ExceptT ApiError m b

type RunEs m a b = (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m) =>
                     EsAction a -> ExceptT ApiError m b

dbInsertRecords
  :: ( MonadIO m
     , MonadReader ApiConfig m
     , MonadBaseControl IO m
     )
  => AppName -> RecordDefinition -> [Record] -> ApiItems2T [ApiError] m [Record]
dbInsertRecords appName def input = do
  valid <- validateManyT def input
  let records = populateDefaults def <$> valid
  savedIds <- runDbT appName (dbAction dbInsert def records)
  saved <- runDbT appName (dbAction dbGetById def savedIds)
  return (fromJust <$> saved)

dbUpdateRecords
  :: ( MonadIO m
     , MonadReader ApiConfig m
     , MonadBaseControl IO m
     )
  => AppName
  -> Bool
  -> RecordDefinition
  -> [Record]
  -> ApiItems2T [ApiError] m [Record]
dbUpdateRecords appName replace def input = do
  valid1 <- validateIdManyT input
  existing <- getExisting appName def $ getIdValue' <$> valid1
  let merged = merge (mkIdIndexedMap valid1) <$> existing
  valid2 <- validateManyT def merged
  let records = populateDefaults def <$> valid2
  savedIds <- runDbT appName (dbAction dbUpdate def records)
  saved <- runDbT appName (dbAction dbGetById def savedIds)
  return (fromJust <$> saved)
  where
    get record = fromJust . Map.lookup (getIdValue' record)
    merge rMap existing = mergeRecords' replace existing (get existing rMap)

getExisting
  :: ( MonadIO m
     , MonadReader ApiConfig m
     , MonadBaseControl IO m
     )
  => AppName
  -> RecordDefinition
  -> [RecordId]
  -> ApiItems2T [ApiError] m [Record]
getExisting appName def ids = do
  records <- runDbT appName (dbAction dbGetById def ids)
  let results = maybe (Left mkApiError404) Right <$> records
  ApiItems2T . return $ itemsFromEither results

-- ^
-- Run multiple MongoDB actions and return all results
dbAction
  :: (Monad m)
  => (RecordDefinition -> b -> Database -> Pipe -> m (Either Failure e))
  -> RecordDefinition
  -> [b]
  -> Database
  -> Pipe
  -> m [Either Failure e]
dbAction singleAction def rs dbName pipe =
  mapM (\r -> singleAction def r dbName pipe) rs

validateManyT
  :: (Monad m)
  => RecordDefinition -> [Record] -> ApiItems2T [ApiError] m [Record]
validateManyT def records =
  ApiItems2T . return . concatItems $
  (vResultToItems . validateRecord def) <$> records

validateIdManyT
  :: (Monad m)
  => [Record] -> ApiItems2T [ApiError] m [Record]
validateIdManyT records =
  ApiItems2T . return . concatItems $
  (vResultToItems . validateRecordHasId) <$> records

runDbT
  :: ( MonadIO m
     , MonadReader ApiConfig m
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
-- runDb :: AppName -> RunDb m a a
runDb
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => AppName
  -> (Database -> Pipe -> m (Either Failure a))
  -> ExceptT ApiError m a
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
validateId' = ExceptT . return . validateId

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

-- ^
-- Make a make a map with the ids as keys and records as values
mkIdIndexedMap :: [Record] -> Map.Map RecordId Record
mkIdIndexedMap = mkRecordMap idLabel

-- ^
-- Merge an existing and an updated record according to the 'replace' flag
mergeRecords' :: Bool -> Record -> Record -> Record
mergeRecords' True = replaceRecords [createdAtLabel, updatedAtLabel, idLabel]
mergeRecords' False = mergeRecords

-- ^
-- Make a make a map keyed by the specified field and having records as values
mkRecordMap :: Label -> [Record] -> Map.Map RecordId Record
mkRecordMap label xs = Map.fromList (addId <$> xs)
  where
    addId r = (getValue' label r, r)
