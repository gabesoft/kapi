{-# LANGUAGE OverloadedStrings #-}

-- | Functionality for interacting with MongoDB
module Persistence.MongoDB where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (unpack, pack)
import Database.MongoDB
import Network.Socket (HostName, PortNumber)
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
-- Create an index on a database using the supplied pipe
dbAddIndex
  :: MonadIO m
  => Database -> Index -> Pipe -> m ()
dbAddIndex dbName idx = dbAccess dbName (createIndex idx)

-- |
-- Insert a record into a database collection and return the _id value
dbInsert
  :: MonadIO m
  => Database -> Collection -> Record -> Pipe -> m RecordId
dbInsert dbName collName doc =
  fmap (fromJust . objIdToRecId) <$>
  dbAccess dbName (insert collName $ recFields (mapIdToObjId doc))

dbUpdate
  :: MonadIO m
  => Database -> Collection -> Record -> Pipe -> m ()
dbUpdate dbName collName doc =
  dbAccess dbName $ save collName (recFields $ mapIdToObjId doc)

-- |
-- Get a record by id
dbGetById
  :: MonadIO m
  => Database -> Collection -> RecordId -> Pipe -> m (Maybe Record)
dbGetById dbName collName recId pipe =
  fmap (mapIdToRecId . Record) <$>
  dbAccess dbName (findOne $ idQuery collName recId) pipe

-- |
-- Delete a record by id
dbDeleteById
  :: MonadIO m
  => Database -> Collection -> RecordId -> Pipe -> m ()
dbDeleteById dbName collName recId =
  dbAccess dbName (delete $ idQuery collName recId)

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
mapIdToRecId = mapField "_id" (>>= objIdToRecId)

-- |
-- Convert the id within a record to an @ObjectId@
mapIdToObjId :: Record -> Record
mapIdToObjId = mapField "_id" (>>= recIdToObjId)
