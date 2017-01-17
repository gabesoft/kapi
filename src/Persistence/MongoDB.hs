{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Functionality for interacting with MongoDB
module Persistence.MongoDB
  ( mkPipe
  , dbAccess
  , dbAddIndex
  , dbCount
  , dbDeleteById
  , dbFind
  , dbGetById
  , dbInsert
  , dbUpdate
  , mkSortField
  , mkIncludeField
  , queryToDoc
  , validateHasId
  , mkOutRecord
  , mkInDocument
  , recordToDocument
  , documentToRecord
  ) where

import Control.Exception.Lifted (handleJust)
import Control.Monad ((>=>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Bifunctor
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Database.MongoDB
import Network.Socket (HostName, PortNumber)
import Parsers.Filter (parse)
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
dbAccess :: MonadIO m => Database -> Action m a -> Pipe -> m a
dbAccess database action pipe = access pipe master database action

-- |
-- Create an index on the database using the supplied pipe
dbAddIndex :: MonadIO m => Database -> Index -> Pipe -> m ()
dbAddIndex dbName idx = dbAccess dbName (createIndex idx)

-- |
-- Insert a record into a collection and return the _id value or a @Failure@
dbInsert :: (MonadIO m, MonadBaseControl IO m) => Database -> RecordDefinition -> Record -> Pipe -> m (Either Failure RecordId)
dbInsert dbName def input pipe =
  dbAction $ do
    doc <- mkInDocument def True input
    docId <- objIdToRecId <$> run doc pipe
    maybe (error "Unexpected missing document") (return . Right) docId
  where
    collName = recordCollection def
    run doc = dbAccess dbName (insert collName doc)

-- |
-- Save an existing record into the database
-- The input record is assumed to have the id field populated
dbUpdate :: (MonadIO m, MonadBaseControl IO m) => Database -> RecordDefinition -> Record -> Pipe -> m (Either Failure RecordId)
dbUpdate dbName def input pipe =
  dbAction $ do
    doc <- mkInDocument def False input
    run doc pipe
    return $ Right (fromJust $ getIdValue input)
  where
    collName = recordCollection def
    run = dbAccess dbName . save collName

-- |
-- Select multiple records
dbFind
  :: (MonadBaseControl IO m, MonadIO m)
  => Database -> RecordDefinition -> [Field] -> [Field] -> [Field] -> Int -> Int -> Pipe -> m [Record]
dbFind dbName def filter' sort' fields skip' limit' pipe =
  fmap (mkOutRecord def) <$> run pipe
  where
    collName = recordCollection def
    run =
      dbAccess dbName $
      find
        (select filter' collName)
        {sort = sort'}
        {project = fields}
        {skip = fromIntegral skip'}
        {limit = fromIntegral limit'} >>= rest

-- |
-- Get a record by id
dbGetById :: MonadIO m => Database -> RecordDefinition -> RecordId -> Pipe -> m (Maybe Record)
dbGetById dbName def recId pipe = fmap (mkOutRecord def) <$> run pipe
  where
    collName = recordCollection def
    run = dbAccess dbName (findOne $ idQuery collName recId)

-- |
-- Delete a record by id
dbDeleteById :: MonadIO m => Database -> RecordDefinition -> RecordId -> Pipe -> m ()
dbDeleteById dbName def recId =
  dbAccess dbName (delete $ idQuery (recordCollection def) recId)

-- |
-- Count the number of records in a collection
dbCount :: MonadIO m => Database -> RecordDefinition -> Pipe -> m Int
dbCount dbName def = dbAccess dbName (count (select [] $ recordCollection def))

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
mkOutRecord :: RecordDefinition -> Document -> Record
mkOutRecord = documentToRecord

-- |
-- Create a document ready to be saved or updated
mkInDocument :: MonadIO m => RecordDefinition -> Bool -> Record -> m Document
mkInDocument def isNew = fmap (recordToDocument def) . setTimestamp' isNew

-- |
-- Convert a BSON @Document@ to a @Record@
documentToRecord :: RecordDefinition -> Document -> Record
documentToRecord def document =
  foldr mapIdToRecId (Record document) ("_id" : getIdLabels def)

-- |
-- Convert a @Record@ to a BSON @Document@
recordToDocument :: RecordDefinition -> Record -> Document
recordToDocument def record =
  getDocument (foldr mapIdToObjId record $ "_id" : getIdLabels def)

-- |
-- Get the labels of all fields of type @ObjectId@ in a @RecordDefinition@
getIdLabels :: RecordDefinition -> [Label]
getIdLabels = Map.keys . Map.filter isObjectId . recordFields

-- |
-- Set the updatedAt field. For new records also set a createdAt field.
setTimestamp' :: MonadIO m => Bool -> Record -> m Record
setTimestamp' isNew =
  if isNew
    then setUpdatedAt >=> setCreatedAt
    else setUpdatedAt

-- |
-- Get an selection for querying one record by id
idQuery
  :: Select a
  => Collection -> RecordId -> a
idQuery collName recId = select ["_id" =: recIdToObjId recId] collName

-- |
-- Convert a record id to an object id value
recIdToObjId :: RecordId -> Maybe Value
recIdToObjId rid = ObjId <$> readMaybe (T.unpack rid)

-- |
-- Convert an object id value to a record id
objIdToRecId :: Value -> Maybe RecordId
objIdToRecId (ObjId v) = Just $ T.pack (show v)
objIdToRecId _ = Nothing

-- |
-- Convert the id within a record to a @RecordId@
mapIdToRecId :: Label -> Record -> Record
mapIdToRecId name = modField name (>>= objIdToRecId)

-- |
-- Convert the id within a record to an @ObjectId@
mapIdToObjId :: Label -> Record -> Record
mapIdToObjId name = modField name (>>= recIdToObjId)

-- |
-- Validate that a record has a valid id field
validateHasId :: Record -> (Record, ValidationResult)
validateHasId r = (r, ValidationErrors $ catMaybes [valField])
  where
    valField = validateField False def (mapIdToObjId "_id" r) "_id"
    def = RecordDefinition mempty $ Map.fromList [mkReqDef' "_id"]

-- |
-- Make a field that will be used for sorting
--
-- >>> mkSortField "+name"
-- [name := 1]
--
-- >>> mkSortField "-name"
-- [name := -1]
--
mkSortField :: Text -> Maybe Field
mkSortField name
  | T.null name = Nothing
  | T.head name == '-' = Just $ mkField (negate 1)
  | otherwise = Just $ mkField 1
  where
    mkField = mkIntField (getName name)
    getName xs
      | T.head xs == '+' || T.head xs == '-' = T.tail xs
      | otherwise = xs

-- |
-- Make a field that will be used for projection during a partial response
mkIncludeField :: Text -> Maybe Field
mkIncludeField name
  | T.null name = Nothing
  | otherwise = Just (mkIntField name 1)

mkIntField :: Text -> Int -> Field
mkIntField = (=:)

-- |
-- Convert a filter query to a @Document@
queryToDoc :: Text -> Either String Document
queryToDoc xs
  | T.null xs = Right []
  | otherwise = first ("Invalid query: " ++) $ parse xs >>= filterToDoc

-- |
-- Convert a @FilterExpr@ into a @Document@ that can be used to filter
-- records during a query
filterToDoc :: FilterExpr -> Either String Document
filterToDoc (FilterRelOp op col term) = (:[]) <$> termToField op col term
filterToDoc (FilterBoolOp op e1 e2) = do
  f1 <- filterToDoc e1
  f2 <- filterToDoc e2
  return [opName op =: [f1, f2]]
  where
    opName And = "$and"
    opName Or = "$or"

-- |
-- Convert a filter term to a field
termToField :: FilterRelationalOperator -> ColumnName -> FilterTerm -> Either String Field
termToField Equal (ColumnName col _) term = Right (col =: term)
termToField NotEqual (ColumnName col _) term = Right (col =: ("$ne" =: term))
termToField In (ColumnName col _) term@(TermList _) = Right (col =: ("$in" =: term))
termToField In (ColumnName col _) term = Right (col =: ("$in" =: TermList [term]))
termToField NotIn (ColumnName col _) term@(TermList _) = Right (col =: ("$nin" =: term))
termToField NotIn (ColumnName col _) term = Right (col =: ("$nin" =: TermList [term]))
termToField GreaterThan (ColumnName col _) term = Right (col =: ("$gt" =: term))
termToField GreaterThanOrEqual (ColumnName col _) term = Right (col =: ("$gte" =: term))
termToField LessThan (ColumnName col _) term = Right (col =: ("$lt" =: term))
termToField LessThanOrEqual (ColumnName col _) term = Right (col =: ("$lte" =: term))
termToField Contains (ColumnName col _) (TermStr s) = Right (col =: Regex s "i")
termToField Contains (ColumnName _ _) _ = Left "contains: expected a string term"
termToField NotContains (ColumnName col _) (TermStr s) = Right (col =: ("$not" =: Regex s "i"))
termToField NotContains (ColumnName _ _) _ = Left "~contains: expected a string term"
