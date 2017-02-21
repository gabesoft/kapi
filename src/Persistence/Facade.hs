{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- ^
-- Facade that provides consistent access to persistence functionality
module Persistence.Facade
  ( dbInsert
  , dbInsertMulti
  , dbUpdate
  , dbUpdateMulti
  , dbPipe
  , getExisting
  , getExistingMulti
  , runDb
  , runEs
  , runEsAndExtract
  , toSingle
  , validate
  , validate'
  , validateId
  , validateId'
  , validateIdMulti
  , validateMulti
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
import qualified Data.Text as T
import Database.Bloodhound.Types (EsError, SearchResult)
import Database.MongoDB (Pipe, Database, Failure)
import Network.HTTP.Types.Status
import Persistence.Common
import qualified Persistence.ElasticSearch as ES
import qualified Persistence.MongoDB as DB
import Types.Common

dbInsert
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => AppName -> RecordDefinition -> Record -> ExceptT ApiError m Record
dbInsert appName = toSingle . dbInsertMulti appName

dbUpdate
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => Bool -> AppName -> RecordDefinition -> Record -> ExceptT ApiError m Record
dbUpdate replace appName = toSingle . dbUpdateMulti replace appName

getExisting
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => AppName -> RecordDefinition -> RecordId -> ExceptT ApiError m Record
getExisting appName = toSingle . getExistingMulti appName

dbInsertMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => AppName -> RecordDefinition -> [Record] -> ApiItems2T [ApiError] m [Record]
dbInsertMulti appName def input = do
  valid <- validateMulti def input
  let records = populateDefaults def <$> valid
  savedIds <- runDbMany appName (dbAction DB.dbInsert def records)
  saved <- runDbMany appName (dbAction DB.dbGetById def savedIds)
  return (fromJust <$> saved)

dbUpdateMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => Bool
  -> AppName
  -> RecordDefinition
  -> [Record]
  -> ApiItems2T [ApiError] m [Record]
dbUpdateMulti replace appName def input = do
  valid1 <- validateIdMulti input
  existing <- getExistingMulti appName def $ getIdValue' <$> valid1
  let merged = merge (mkIdIndexedMap valid1) <$> existing
  valid2 <- validateMulti def merged
  let records = populateDefaults def <$> valid2
  savedIds <- runDbMany appName (dbAction DB.dbUpdate def records)
  saved <- runDbMany appName (dbAction DB.dbGetById def savedIds)
  return (fromJust <$> saved)
  where
    get record = fromJust . Map.lookup (getIdValue' record)
    merge rMap existing = mergeRecords' replace existing (get existing rMap)

getExistingMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => AppName
  -> RecordDefinition
  -> [RecordId]
  -> ApiItems2T [ApiError] m [Record]
getExistingMulti appName def ids = do
  records <- runDbMany appName (dbAction DB.dbGetById def ids)
  let msg =
        "Record not found in " ++
        T.unpack (recordCollectionName def) ++ " collection"
  let results = maybe (Left $ mkApiError404' msg) Right <$> records
  ApiItems2T . return $ eitherToItems results

-- ^
-- Convert an action operating on multiple records into one operating
-- on a single record
toSingle
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => ([a] -> ApiItems2T [e] m [b]) -> a -> ExceptT e m b
toSingle action input =
  ExceptT $ do
    records <- runApiItems2T $ action [input]
    return $ head (itemsToEither records)

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

-- ^
-- Run a MongoDB action that handle multiple records
runDbMany
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => AppName
  -> (Database -> Pipe -> m [Either Failure a])
  -> ApiItems2T [ApiError] m [a]
runDbMany appName action = do
  results <- runDb' appName action
  let items = first DB.dbToApiError <$> results
  ApiItems2T . return $ ApiItems2 (lefts items) (rights items)

-- ^
-- Run a MongoDB action that handles a single record
runDb
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => AppName
  -> (Database -> Pipe -> m (Either Failure a))
  -> ExceptT ApiError m a
runDb appName action = do
  results <- runDb' appName action
  ExceptT (return $ first DB.dbToApiError results)

runDb'
  :: (Monad m, MonadTrans t, MonadIO (t m), MonadReader ApiConfig (t m))
  => AppName -> (Database -> Pipe -> m b) -> t m b
runDb' appName action = do
  conf <- ask
  pipe <- dbPipe conf
  lift $ action (confGetDb appName conf) pipe

-- ^
-- Run an Elastic-Search action
runEs
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => AppName -> (Text -> Text -> IO (Either EsError a)) -> ExceptT ApiError m a
runEs appName action = do
  conf <- ask
  results <- liftIO $ action (esServer conf) (confGetEsIndex appName conf)
  ExceptT (return $ first ES.esToApiError results)

runEsAndExtract
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => AppName
  -> (Text -> Text -> IO (Either EsError (SearchResult Record)))
  -> ExceptT ApiError m [Record]
runEsAndExtract appName = fmap (ES.extractRecords []) . runEs appName

-- ^
-- Ensure that a record is valid according to its definition
validate :: RecordDefinition -> Record -> Either ApiError Record
validate def = vResultToEither . validateRecord def

-- ^
-- Ensure that a record has a valid id
validateId :: Record -> Either ApiError Record
validateId = vResultToEither . DB.validateRecordHasId

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
-- Validate multiple records against their definition
validateMulti
  :: (Monad m)
  => RecordDefinition -> [Record] -> ApiItems2T [ApiError] m [Record]
validateMulti def = validateMulti' (validateRecord def)

-- ^
-- Ensure that all records specified have a valid id
validateIdMulti
  :: (Monad m)
  => [Record] -> ApiItems2T [ApiError] m [Record]
validateIdMulti = validateMulti' DB.validateRecordHasId

validateMulti'
  :: (Monad m)
  => (a -> (Record, ValidationResult))
  -> [a]
  -> ApiItems2T [ApiError] m [Record]
validateMulti' v records =
  ApiItems2T . return . concatItems $ (vResultToItems . v) <$> records

-- ^
-- Convert the result of a validation to 'Either'
vResultToEither :: (Record, ValidationResult) -> Either ApiError Record
vResultToEither (a, ValidationErrors []) = Right a
vResultToEither (a, err) = Left (ApiError (Just a) status400 (encode err))

vResultToItems :: (Record, ValidationResult) -> ApiResults2
vResultToItems (a, ValidationErrors []) = ApiItems2 [] [a]
vResultToItems (a, err) = ApiItems2 [ApiError (Just a) status400 (encode err)] []

-- ^
-- Create a MongoDB connection pipe
dbPipe
  :: MonadIO m
  => ApiConfig -> m Pipe
dbPipe conf = liftIO $ DB.mkPipe (mongoHost conf) (mongoPort conf)

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
