{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functionality for interacting with MongoDB
module Persistence.MongoDB where

import Control.Exception.Lifted (handleJust)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (unpack, pack)
import Database.MongoDB
import Network.Socket (HostName, PortNumber)
import Persistence.Common
import Text.Read (readMaybe)
import Types.Common

-- |
-- Create a database connection
-- >>> pipe <- mkPipe "127.0.0.1" 27017
-- >>> close pipe
mkPipe :: HostName -> PortNumber -> IO Pipe
mkPipe addr port = connect (Host addr $ PortNumber port)

-- |
-- Perform an action with master access on a database
-- using the supplied connection pipe
dbAccess
  :: MonadIO m
  => Database -> Action m a -> Pipe -> m a
dbAccess database action pipe = access pipe master database action

-- |
-- Create an index on the database using the supplied pipe
dbAddIndex
  :: MonadIO m
  => Database -> Index -> Pipe -> m ()
dbAddIndex dbName idx = dbAccess dbName (createIndex idx)

-- |
-- Insert a record into a collection and return the _id value
dbInsert'
  :: MonadIO m
  => Database -> Collection -> Record -> Pipe -> m (Maybe RecordId)
dbInsert' dbName collName input pipe = do
  doc <- mkInDocument input
  objId <- dbAccess dbName (insert collName doc) pipe
  return (objIdToRecId objId)

-- |
-- Insert a record into a collection and return the _id value or a @Failure@
dbInsert
  :: (MonadIO m, MonadBaseControl IO m)
  => Database -> Collection -> Record -> Pipe -> m (Either Failure RecordId)
dbInsert dbName collName doc pipe =
  dbAction $
  do maybeId <- dbInsert' dbName collName doc pipe
     maybe (error "Unexpected missing document") (return . Right) maybeId

-- |
-- Save an existing record into the database
-- The input record is assumed to have the id field populated
dbUpdate'
  :: MonadIO m
  => Database -> Collection -> Record -> Pipe -> m RecordId
dbUpdate' dbName collName input pipe = do
  doc <- mkInDocument input
  dbAccess dbName (save collName doc) pipe
  return (fromJust $ getIdValue input)

-- |
-- Save an existing record or insert a new record into the database
dbUpdate
  :: (MonadIO m, MonadBaseControl IO m)
  => Database -> Collection -> Record -> Pipe -> m (Either Failure RecordId)
dbUpdate dbName collName doc pipe =
  dbAction $ Right <$> dbUpdate' dbName collName doc pipe

-- |
-- Select multiple records
dbFind
  :: (MonadBaseControl IO f, MonadIO f)
  => Database -> Collection -> Pipe -> f [Record]
dbFind dbName collName pipe = do
  docs <- dbAccess dbName (find (select [] collName) >>= rest) pipe
  mapM mkOutRecord docs

-- |
-- Get a record by id
dbGetById
  :: MonadIO m
  => Database -> Collection -> RecordId -> Pipe -> m (Maybe Record)
dbGetById dbName collName recId pipe = do
  doc <- dbAccess dbName (findOne $ idQuery collName recId) pipe
  maybe (return Nothing) (fmap Just . mkOutRecord) doc

-- |
-- Delete a record by id
dbDeleteById
  :: MonadIO m
  => Database -> Collection -> RecordId -> Pipe -> m ()
dbDeleteById dbName collName recId =
  dbAccess dbName (delete $ idQuery collName recId)

-- |
-- Perform a database action that could result in a @Failure@
dbAction :: (MonadIO m, MonadBaseControl IO m) => m (Either Failure a) -> m (Either Failure a)
dbAction = handleJust failureHandler (return . Left)

-- |
-- Handler for MongoDB errors
failureHandler :: Failure -> Maybe Failure
failureHandler err@WriteFailure {} = Just err
failureHandler _ = Nothing

-- |
-- Create a record ready to be returned from a query action
mkOutRecord :: MonadIO m => Document -> m Record
mkOutRecord = fmap mapIdToRecId . setCreatedAt . Record

-- |
-- Create a document ready to be saved or updated
mkInDocument :: MonadIO m => Record -> m Document
mkInDocument = fmap getDocument . setUpdatedAt . delField "_createdAt" . mapIdToObjId

-- |
-- Get an selection for querying one record by id
idQuery
  :: Select a
  => Collection -> RecordId -> a
idQuery collName recId = select ["_id" =: recIdToObjId recId] collName

-- |
-- Convert a record id to an object id value
recIdToObjId :: RecordId -> Maybe Value
recIdToObjId rid = ObjId <$> readMaybe (unpack rid)

-- |
-- Convert an object id value to a record id
objIdToRecId :: Value -> Maybe RecordId
objIdToRecId (ObjId v) = Just $ pack (show v)
objIdToRecId _ = Nothing

-- |
-- Convert the id within a record to a @RecordId@
mapIdToRecId :: Record -> Record
mapIdToRecId = modField "_id" (>>= objIdToRecId)

-- |
-- Convert the id within a record to an @ObjectId@
mapIdToObjId :: Record -> Record
mapIdToObjId = modField "_id" (>>= recIdToObjId)

-- |
-- Validate that a record has a valid id field
validateHasId :: Record -> (Record, ValidationResult)
validateHasId r = (r, ValidationErrors $ catMaybes [valField])
  where
    valField = validateField False def (mapIdToObjId r) "_id"
    def = Map.fromList [mkReqDef' "_id"]
