{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Functionality for interacting with MongoDB
module Persistence.MongoDB
  ( mkPipe
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

-- ^
-- Create a database connection
-- >>> pipe <- mkPipe "127.0.0.1" 27017
-- >>> close pipe
mkPipe :: HostName -> PortNumber -> IO Pipe
mkPipe addr port = connect (Host addr $ PortNumber port)

-- ^
-- Perform an action with master access on a database
-- using the supplied connection pipe
dbAccess :: MonadIO m => Action m a -> Database -> Pipe -> m a
dbAccess action dbName pipe = access pipe master dbName action

-- ^
-- Create an index on the database using the supplied pipe
dbAddIndex :: MonadIO m => Index -> Database -> Pipe -> m ()
dbAddIndex idx = dbAccess (createIndex idx)

-- ^
-- Insert a record into a collection and return the _id value or a @Failure@
dbInsert :: (MonadIO m, MonadBaseControl IO m) => RecordDefinition -> Record -> Database -> Pipe -> m (Either Failure RecordId)
dbInsert def input dbName pipe =
  dbAction $ do
    doc <- mkInDocument def True input
    docId <- objIdToRecId <$> run doc pipe
    maybe (error "Unexpected missing document") (return . Right) docId
  where
    collName = recordCollection def
    run doc = dbAccess (insert collName doc) dbName

-- ^
-- Save an existing record into the database
-- The input record is assumed to have the id field populated
dbUpdate :: (MonadIO m, MonadBaseControl IO m) => RecordDefinition -> Record -> Database -> Pipe -> m (Either Failure RecordId)
dbUpdate def input dbName pipe =
  dbAction $ do
    doc <- mkInDocument def False input
    run doc pipe
    return $ Right (fromJust $ getIdValue input)
  where
    collName = recordCollection def
    run doc = dbAccess (save collName doc) dbName

-- ^
-- Select multiple records
dbFind
  :: (MonadBaseControl IO m, MonadIO m)
  => RecordDefinition -> [Field] -> [Field] -> [Field] -> Int -> Int -> Database -> Pipe -> m [Record]
dbFind def filter' sort' fields skip' limit' dbName pipe =
  fmap (mkOutRecord def) <$> run pipe
  where
    collName = recordCollection def
    run = dbAccess action dbName
    action =
      find
        (select filter' collName)
        {sort = sort'}
        {project = fields}
        {skip = fromIntegral skip'}
        {limit = fromIntegral limit'} >>= rest

-- ^
-- Get a record by id
dbGetById :: MonadIO m => RecordDefinition -> RecordId -> Database -> Pipe -> m (Maybe Record)
dbGetById def recId dbName pipe = fmap (mkOutRecord def) <$> run pipe
  where
    collName = recordCollection def
    run = dbAccess (findOne $ idQuery collName recId) dbName

-- ^
-- Delete a record by id
dbDeleteById :: MonadIO m => RecordDefinition -> RecordId -> Database -> Pipe -> m ()
dbDeleteById def recId = dbAccess (delete $ idQuery (recordCollection def) recId)

-- ^
-- Count the number of records in a collection
dbCount :: MonadIO m => RecordDefinition -> Database -> Pipe -> m Int
dbCount def = dbAccess (count (select [] $ recordCollection def))

-- ^
-- Perform a database action that could result in a @Failure@
dbAction :: (MonadIO m, MonadBaseControl IO m) => m (Either Failure a) -> m (Either Failure a)
dbAction = handleJust failureHandler (return . Left)

-- ^
-- Handler for MongoDB errors
failureHandler :: Failure -> Maybe Failure
failureHandler err@WriteFailure {} = Just err
failureHandler _ = Nothing

-- ^
-- Create a record ready to be returned from a query action
mkOutRecord :: RecordDefinition -> Document -> Record
mkOutRecord = documentToRecord

-- ^
-- Create a document ready to be saved or updated
mkInDocument :: MonadIO m => RecordDefinition -> Bool -> Record -> m Document
mkInDocument def isNew = fmap (recordToDocument def) . setTimestamp' isNew

-- ^
-- Convert a BSON @Document@ to a @Record@
documentToRecord :: RecordDefinition -> Document -> Record
documentToRecord def document =
  foldr mapIdToRecId (Record document) (idLabel : getIdLabels def)

-- ^
-- Convert a @Record@ to a BSON @Document@
recordToDocument :: RecordDefinition -> Record -> Document
recordToDocument def record =
  getDocument (foldr mapIdToObjId record $ idLabel : getIdLabels def)

-- ^
-- Get the labels of all fields of type @ObjectId@ in a @RecordDefinition@
getIdLabels :: RecordDefinition -> [Label]
getIdLabels = Map.keys . Map.filter isObjectId . recordFields

-- ^
-- Set the updatedAt field. For new records also set a createdAt field.
setTimestamp' :: MonadIO m => Bool -> Record -> m Record
setTimestamp' isNew =
  if isNew
    then setUpdatedAt >=> setCreatedAt
    else setUpdatedAt

-- ^
-- Get an selection for querying one record by id
idQuery
  :: Select a
  => Collection -> RecordId -> a
idQuery collName recId = select [idLabel =: recIdToObjId recId] collName

-- ^
-- Convert a record id to an object id value
recIdToObjId :: RecordId -> Maybe Value
recIdToObjId rid = ObjId <$> readMaybe (T.unpack rid)

-- ^
-- Convert an object id value to a record id
objIdToRecId :: Value -> Maybe RecordId
objIdToRecId (ObjId v) = Just $ T.pack (show v)
objIdToRecId _ = Nothing

-- ^
-- Convert the id within a record to a @RecordId@
mapIdToRecId :: Label -> Record -> Record
mapIdToRecId name = modField name (>>= objIdToRecId)

-- ^
-- Convert the id within a record to an @ObjectId@
mapIdToObjId :: Label -> Record -> Record
mapIdToObjId name = modField name (>>= recIdToObjId)

-- ^
-- Validate that a record has a valid id field
validateHasId :: Record -> (Record, ValidationResult)
validateHasId r = (r, ValidationErrors $ catMaybes [valField])
  where
    valField = validateField False def (mapIdToObjId idLabel r) idLabel
    def = RecordDefinition mempty $ Map.fromList [mkReqDef' idLabel]

-- ^
-- Make a field that will be used for sorting
--
-- >>> mkSortField "+name"
-- [name := 1]
--
-- >>> mkSortField "-name"
-- [name := -1]
--
mkSortField :: Text -> Maybe Field
mkSortField = fmap mkField . mkSortExpr
  where
    mkField (SortExpr n SortAscending) = mkIntField n 1
    mkField (SortExpr n SortDescending) = mkIntField n (negate 1)

-- ^
-- Make a field that will be used for projection during a partial response
mkIncludeField :: Text -> Maybe Field
mkIncludeField name
  | T.null name = Nothing
  | otherwise = Just (mkIntField name 1)

mkIntField :: Text -> Int -> Field
mkIntField = (=:)

-- ^
-- Convert a filter query to a @Document@
queryToDoc :: Text -> Either String Document
queryToDoc xs
  | T.null xs = Right []
  | otherwise = first ("Invalid query: " ++) $ parse xs >>= filterToDoc

-- ^
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

-- ^
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
