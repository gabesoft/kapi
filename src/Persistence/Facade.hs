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
  , mergeFromMap
  , runDb
  , runEs
  , runEsAndExtract
  , toMulti
  , toSingle
  , validate
  , validate'
  , validateId
  , validateId'
  , validateDbIdMulti
  , validateEsIdMulti
  , validateMulti
  ) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson (encode)
import Data.Bifunctor
import Data.Bson (Label, (=:))
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
  => RecordDefinition -> Record -> ExceptT ApiError m Record
dbInsert = toSingle . dbInsertMulti

dbUpdate
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => Bool -> RecordDefinition -> Record -> ExceptT ApiError m Record
dbUpdate replace = toSingle . dbUpdateMulti replace

getExisting
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => RecordDefinition -> RecordId -> ExceptT ApiError m Record
getExisting = toSingle . getExistingMulti

dbInsertMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => RecordDefinition -> [Record] -> ApiItems2T [ApiError] m [Record]
dbInsertMulti def input = do
  valid <- validateMulti def input
  let records = populateDefaults def <$> valid
  savedIds <- runDbMany (dbAction DB.dbInsert def records)
  saved <- runDbMany (dbAction DB.dbGetById def savedIds)
  return (fromJust <$> saved)

dbUpdateMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => Bool -> RecordDefinition -> [Record] -> ApiItems2T [ApiError] m [Record]
dbUpdateMulti replace def input = do
  valid1 <- validateDbIdMulti input
  existing <- getExistingMulti def $ getIdValue' <$> valid1
  let merged = mergeFromMap replace (mkIdIndexedMap valid1) <$> existing
  valid2 <- validateMulti def merged
  let records = populateDefaults def <$> valid2
  savedIds <- runDbMany (dbAction DB.dbUpdate def records)
  saved <- runDbMany (dbAction DB.dbGetById def savedIds)
  return (fromJust <$> saved)

-- ^
-- Merge a new record from the given map with the specified existing record
mergeFromMap :: Bool -> Map.Map RecordId Record -> Record -> Record
mergeFromMap replace newMap existing = mergeRecords' replace existing new
  where
    get r = fromJust . Map.lookup (getIdValue' r)
    new = get existing newMap

getExistingMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => RecordDefinition -> [RecordId] -> ApiItems2T [ApiError] m [Record]
getExistingMulti def ids = do
  records <- runDbMany (dbAction DB.dbGetById def ids)
  let tuples = zip records ids
  let result (record, rid) = maybe (Left $ mk404Err def rid) Right record
  ApiItems2T . return $ eitherToItems (result <$> tuples)

-- ^
-- Make a 404 error to be returned when attempting to construct an invalid user post
-- TODO: consolidate error message with same from 'getExistingMulti'
-- TODO: consolidate with other errors
mk404Err :: RecordDefinition -> RecordId -> ApiError
mk404Err def rid =
  ApiError (Just $ Record [ idLabel =: rid ]) status400 $
  LBS.pack $
  "Record not found in " ++
  T.unpack (recordCollectionName def) ++ " collection."

-- ^
-- TODO: rename
-- Run an action that could result in a single failure
toSingle
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => ([a] -> ApiItems2T [e] m [b]) -> a -> ExceptT e m b
toSingle action input =
  ExceptT $ do
    records <- runApiItems2T $ action [input]
    return $ head (itemsToEither records)

-- ^
-- TODO: rename
-- Run an action that could result in multiple failures
toMulti
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => ExceptT a (ApiItems2T [a] m) [t] -> ApiItems2T [a] m [t]
toMulti action = do
  results <- runExceptT action
  ApiItems2T . return $
    either (flip ApiItems2 [] . (: [])) (ApiItems2 []) results

-- ^
-- Run a MongoDB action that could result in multiple errors
runDbMany
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => (Database -> Pipe -> m [Either Failure a]) -> ApiItems2T [ApiError] m [a]
runDbMany action = do
  results <- runDb' action
  let items = first DB.dbToApiError <$> results
  ApiItems2T . return $ ApiItems2 (lefts items) (rights items)

-- ^
-- Run a MongoDB action that could result in a single error
runDb
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => (Database -> Pipe -> m (Either Failure a)) -> ExceptT ApiError m a
runDb action = do
  results <- runDb' action
  ExceptT (return $ first DB.dbToApiError results)

runDb'
  :: (Monad m, MonadTrans t, MonadIO (t m), MonadReader ApiConfig (t m))
  => (Database -> Pipe -> m b) -> t m b
runDb' action = do
  conf <- ask
  let app = appName conf
  pipe <- dbPipe conf
  lift $ action (confGetDb app conf) pipe

-- ^
-- Run an elastic-search action
runEs
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => (Text -> Text -> IO (Either EsError a)) -> ExceptT ApiError m a
runEs action = do
  conf <- ask
  let app = appName conf
  results <- liftIO $ action (esServer conf) (confGetEsIndex app conf)
  ExceptT (return $ first ES.esToApiError results)

-- ^
-- Run a search action and extract the results
runEsAndExtract
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => (Text -> Text -> IO (Either EsError (SearchResult Record)))
  -> ExceptT ApiError m [Record]
runEsAndExtract = fmap (ES.extractRecords []) . runEs

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
validateDbIdMulti
  :: (Monad m)
  => [Record] -> ApiItems2T [ApiError] m [Record]
validateDbIdMulti = validateMulti' DB.validateRecordHasId

-- ^
-- Ensure that all records specified have a valid id
validateEsIdMulti
  :: (Monad m)
  => [Record] -> ApiItems2T [ApiError] m [Record]
validateEsIdMulti = validateMulti' ES.validateRecordHasId

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
vResultToItems (a, err) =
  ApiItems2 [ApiError (Just a) status400 (encode err)] []

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
mergeRecords' True = replaceRecords [createdAtLabel, idLabel]
mergeRecords' False = mergeRecords

-- ^
-- Make a make a map keyed by the specified field and having records as values
mkRecordMap :: Label -> [Record] -> Map.Map RecordId Record
mkRecordMap label xs = Map.fromList (addId <$> xs)
  where
    addId r = (getValue' label r, r)
