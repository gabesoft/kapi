{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- ^
-- Facade that provides consistent access to persistence functionality
module Persistence.Facade
  ( dbDelete
  , dbDeleteByQuery
  , dbDeleteMulti
  , dbGetExisting
  , dbGetExistingMulti
  , dbInsert
  , dbInsertMulti
  , dbModify
  , dbModifyMulti
  , dbPipe
  , dbReplace
  , dbReplaceMulti
  , dbUpdateMulti
  , esGetExisting
  , esGetExistingMulti
  , mergeFromMap
  , mkIdIndexedMap
  , mkRecordMap
  , runAction
  , runDb
  , runEs
  , runEsAndExtract
  , toMulti
  , toSingle
  , validateDbIdMulti
  , validateEsIdMulti
  , validateMulti
  , validateMulti'
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson (encode)
import Data.Bifunctor
import Data.Bson (Label, Field)
import Data.Either
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import Database.MongoDB (Pipe, Database, Failure)
import Database.V5.Bloodhound.Types (EsError, SearchResult)
import Network.HTTP.Types.Status
import Persistence.Common
import qualified Persistence.ElasticSearch as ES
import qualified Persistence.MongoDB as DB
import Types.Common
import Util.Constants
import Util.Error

dbInsert
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => RecordDefinition -> Record -> ExceptT ApiError m Record
dbInsert = toSingle . dbInsertMulti

dbReplace
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => RecordDefinition -> Record -> ExceptT ApiError m Record
dbReplace = toSingle . dbReplaceMulti

dbModify
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => RecordDefinition -> Record -> ExceptT ApiError m Record
dbModify = toSingle . dbModifyMulti

dbDelete
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => RecordDefinition -> RecordId -> ExceptT ApiError m Record
dbDelete = toSingle . dbDeleteMulti

dbDeleteByQuery
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => RecordDefinition -> [Field] -> ExceptT ApiError m ()
dbDeleteByQuery def query = runDb (DB.dbDeleteByQuery def query)

dbGetExisting
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => RecordDefinition -> RecordId -> ExceptT ApiError m Record
dbGetExisting = toSingle . dbGetExistingMulti

esGetExisting
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => RecordDefinition -> RecordId -> ExceptT ApiError m Record
esGetExisting = toSingle . esGetExistingMulti

dbReplaceMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => RecordDefinition -> [Record] -> ApiResultsT m
dbReplaceMulti = dbUpdateMulti True

dbModifyMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => RecordDefinition -> [Record] -> ApiResultsT m
dbModifyMulti = dbUpdateMulti False

-- ^
-- Insert multiple records
dbInsertMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => RecordDefinition -> [Record] -> ApiResultsT m
dbInsertMulti _ [] = return []
dbInsertMulti def input = do
  valid <- validateMulti def input
  let records = populateDefaults def <$> valid
  savedIds <- runDbMulti (dbAction DB.dbInsert def records)
  saved <- runDbMulti (dbAction DB.dbGetById def savedIds)
  return (fromJust <$> saved)

-- ^
-- Update multiple records
dbUpdateMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => Bool -> RecordDefinition -> [Record] -> ApiResultsT m
dbUpdateMulti _ _ [] = return []
dbUpdateMulti replace def input = do
  valid1 <- validateDbIdMulti input
  existing <- dbGetExistingMulti def $ getIdValue' <$> valid1
  let merged = mergeFromMap replace (mkIdIndexedMap valid1) <$> existing
  valid2 <- validateMulti def merged
  let records = populateDefaults def <$> valid2
  savedIds <- runDbMulti (dbAction DB.dbUpdate def records)
  saved <- runDbMulti (dbAction DB.dbGetById def savedIds)
  return (fromJust <$> saved)

-- ^
-- Delete multiple records
dbDeleteMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => RecordDefinition -> [RecordId] -> ApiItemsT [ApiError] m [Record]
dbDeleteMulti def ids = do
  existing <- dbGetExistingMulti def ids
  _ <- runDbMulti (dbAction DB.dbDeleteById def ids)
  return existing

-- ^
-- Merge a new record from the given map with the specified existing record
mergeFromMap :: Bool -> Map.Map RecordId Record -> Record -> Record
mergeFromMap replace newMap existing = mergeRecords' replace existing new
  where
    get r = fromJust . Map.lookup (getIdValue' r)
    new = get existing newMap

-- ^
-- Get multiple records by id
dbGetExistingMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => RecordDefinition -> [RecordId] -> ApiResultsT m
dbGetExistingMulti def ids = do
  records <- runDbMulti (dbAction DB.dbGetById def ids)
  let tuples = zip records ids
  let result (record, rid) = maybe (Left $ mk404IdErr def rid) Right record
  eitherToItemsT (result <$> tuples)

-- ^
-- Get multiple records by id
esGetExistingMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => RecordDefinition -> [RecordId] -> ApiResultsT m
esGetExistingMulti def ids = do
  existing <- toMulti $ runEsAndExtract (ES.getByIds ids $ recordCollection def)
  let idSet = Set.fromList ids
  let result r =
        if Set.member (getIdValue' r) idSet
          then Right r
          else Left (mk404IdErr def $ getIdValue' r)
  eitherToItemsT (result <$> existing)

-- ^
-- Convert an action that could result in multiple errors
-- into one that could result in a single error
toSingle
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => ([a] -> ApiItemsT [e] m [b]) -> a -> ExceptT e m b
toSingle action input =
  ExceptT $ do
    records <- runApiItemsT $ action [input]
    return $ head (itemsToEither records)

-- ^
-- Convert an action that could result in a single error
-- into one that could result in multiple errors
toMulti
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => ExceptT a (ApiItemsT [a] m) [t] -> ApiItemsT [a] m [t]
toMulti action = do
  results <- runExceptT action
  ApiItemsT . return $
    either (flip ApiItems [] . (: [])) (ApiItems []) results

-- ^
-- Run a MongoDB action that could result in multiple errors
runDbMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => (Database -> Pipe -> m [Either Failure a]) -> ApiItemsT [ApiError] m [a]
runDbMulti action = do
  results <- runDb' action
  let items = first DB.dbToApiError <$> results
  ApiItemsT . return $ ApiItems (lefts items) (rights items)

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
  pipe <- dbPipe conf
  lift $ action (confGetDbName conf) pipe

-- ^
-- Run an elastic-search action
runEs
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => (Text -> Text -> IO (Either EsError a)) -> ExceptT ApiError m a
runEs action = do
  conf <- ask
  results <- liftIO $ action (esServer conf) (confGetEsIndex conf)
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
-- Run an action over a list of inputs and return all
-- results and errors
runAction
  :: Monad m
  => (a -> m (Either e b)) -> [a] -> ApiItemsT [e] m [b]
runAction f input = lift (mapM f input) >>= eitherToItemsT

-- ^
-- Validate multiple records against their definition
validateMulti
  :: Monad m
  => RecordDefinition -> [Record] -> ApiItemsT [ApiError] m [Record]
validateMulti def = validateMulti' id (validateRecord def)

-- ^
-- Ensure that all records specified have a valid id
validateDbIdMulti
  :: Monad m
  => [Record] -> ApiItemsT [ApiError] m [Record]
validateDbIdMulti = validateMulti' id DB.validateRecordHasId

-- ^
-- Ensure that all records specified have a valid id
validateEsIdMulti
  :: Monad m
  => [Record] -> ApiItemsT [ApiError] m [Record]
validateEsIdMulti = validateMulti' id ES.validateRecordHasId

validateMulti'
  :: (Monad m)
  => (a -> Record)
  -> (a -> (a, ValidationResult))
  -> [a]
  -> ApiItemsT [ApiError] m [a]
validateMulti' f v records =
  ApiItemsT . return . concatItems $ vResultToItems f . v <$> records

vResultToItems :: (a -> Record)
               -> (a, ValidationResult)
               -> ApiItems [ApiError] [a]
vResultToItems _ (a, ValidationErrors []) = ApiItems [] [a]
vResultToItems f (a, err) =
  ApiItems [ApiError (Just $ f a) status400 (encode err)] []

-- ^
-- Create a MongoDB connection pipe
dbPipe
  :: MonadIO m
  => ApiConfig -> m Pipe
dbPipe conf = liftIO $ DB.mkPipe (mongoServer conf)

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
