{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Functionality for interacting with MongoDB
module Persistence.MongoDB
  ( dbAddIndex
  , dbCount
  , dbDeleteById
  , dbFind
  , dbGetById
  , dbInsert
  , dbToApiError
  , dbUpdate
  , documentToRecord
  , idQuery
  , idsQuery
  , mkInDocument
  , mkIncludeField
  , mkIncludeFields
  , mkOutRecord
  , mkPipe
  , mkSortField
  , mkSortFields
  , queryToDoc
  , recordToDocument
  , validateHasId
  ) where

import Control.Exception.Lifted (handleJust)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Bifunctor
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Database.MongoDB
import Network.HTTP.Types.Status
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
-- Insert a record into a collection and return the generated id
-- dbInsert
dbInsert
  :: (MonadBaseControl IO m, MonadIO m)
  => RecordDefinition
  -> Record
  -> Database
  -> Pipe
  -> m (Either Failure RecordId)
dbInsert def record dbName pipe =
  dbAction $
  do input <- mkInDocument def True record
     saved <- dbAccess (action input) dbName pipe
     let maybeId = objIdToRecId saved
     return $ maybe (error "Unexpected missing document") Right maybeId
  where
    action = insert (recordCollection def)

-- ^
-- Save an existing record into the database
-- The input record is assumed to have the id field populated
dbUpdate
  :: (MonadIO m, MonadBaseControl IO m)
  => RecordDefinition
  -> Record
  -> Database
  -> Pipe
  -> m (Either Failure RecordId)
dbUpdate def input dbName pipe =
  dbAction $
  do doc <- mkInDocument def False input
     dbAccess (action doc) dbName pipe
     return $ Right (fromJust $ getIdValue input)
  where
    action = save (recordCollection def)

-- ^
-- Select multiple records
dbFind
  :: (MonadBaseControl IO m, MonadIO m)
  => RecordDefinition
  -> [Field]
  -> [Field]
  -> [Field]
  -> Int
  -> Int
  -> Database
  -> Pipe
  -> m (Either Failure [Record])
dbFind def filter' sort' fields skip' limit' dbName pipe =
  dbAction $
  do docs <- dbAccess action dbName pipe
     return $ Right (mkOutRecord def <$> docs)
  where
    collName = recordCollection def
    action =
      find
        (select filter' collName)
        { sort = sort'}
        { project = fields}
        { skip = fromIntegral skip'}
        { limit = fromIntegral limit'} >>= rest

-- ^
-- Get a record by id
dbGetById
  :: (MonadBaseControl IO m, MonadIO m)
  => RecordDefinition
  -> RecordId
  -> Database
  -> Pipe
  -> m (Either Failure (Maybe Record))
dbGetById def recId dbName pipe =
  dbAction $
  do record <- dbAccess action dbName pipe
     return $ Right (mkOutRecord def <$> record)
  where
    collName = recordCollection def
    action = findOne $ select (idQuery recId) collName

-- ^
-- Delete a record by id
dbDeleteById
  :: (MonadBaseControl IO m, MonadIO m)
  => RecordDefinition -> RecordId -> Database -> Pipe -> m (Either Failure ())
dbDeleteById def recId dbName pipe =
  dbAction $
  do dbAccess action dbName pipe
     return $ Right ()
  where
    action = delete $ select (idQuery recId) (recordCollection def)

-- ^
-- Count the number of records in a collection
dbCount
  :: (MonadBaseControl IO m, MonadIO m)
  => RecordDefinition -> Database -> Pipe -> m (Either Failure Int)
dbCount def dbName pipe = dbAction $ Right <$> dbAccess action dbName pipe
  where
    action = count (select [] $ recordCollection def)

-- ^
-- Perform a database action that could result in a 'Failure'
dbAction :: (MonadIO m, MonadBaseControl IO m) => m (Either Failure a) -> m (Either Failure a)
dbAction = handleJust Just (return . Left)

-- ^
-- Create a record ready to be returned from a query action
mkOutRecord :: RecordDefinition -> Document -> Record
mkOutRecord = documentToRecord

-- ^
-- Create a document ready to be saved or updated
mkInDocument :: MonadIO m => RecordDefinition -> Bool -> Record -> m Document
mkInDocument def isNew = fmap (recordToDocument def) . setTimestamp isNew

-- ^
-- Convert a BSON 'Document' to a 'Record'
documentToRecord :: RecordDefinition -> Document -> Record
documentToRecord def document =
  foldr mapIdToRecId (Record document) (idLabel : getIdLabels def)

-- ^
-- Convert a 'Record' to a BSON 'Document'
recordToDocument :: RecordDefinition -> Record -> Document
recordToDocument def record =
  getDocument (foldr mapIdToObjId record $ idLabel : getIdLabels def)

-- ^
-- Convert all id fields to ObjectId
convertIds :: RecordDefinition -> Record -> Record
convertIds def record = foldr mapIdToObjId record $ getIdLabels def

-- ^
-- Get the labels of all fields of type 'ObjectId' in a 'RecordDefinition'
getIdLabels :: RecordDefinition -> [Label]
getIdLabels = Map.keys . Map.filter isObjectId . recordFields

-- ^
-- Get a query for finding one record by id
idQuery :: RecordId -> [Field]
idQuery recId = [idLabel =: recIdToObjId recId]

-- ^
-- Get a query for finding multiple records by id
idsQuery :: [RecordId] -> [Field]
idsQuery recIds = [idLabel =: ("$in" =: recIdToObjId <$> recIds)]

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
-- Convert the id within a record to a 'RecordId'
mapIdToRecId :: Label -> Record -> Record
mapIdToRecId name = modField name (>>= objIdToRecId)

-- ^
-- Convert the id within a record to an 'ObjectId'
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

-- ^
-- Create a document to be used for filtering a database collection
mkIncludeFields :: [Label] -> [Field]
mkIncludeFields = catMaybes . fmap mkIncludeField . mkIncludeLabels

-- ^
-- Create a document to be used for sorting the result of a query
mkSortFields :: [Label] -> [Field]
mkSortFields = catMaybes . fmap mkSortField

-- ^
-- Create an 'Int' field
mkIntField :: Text -> Int -> Field
mkIntField = (=:)

-- ^
-- Convert a filter query to a 'Document' and convert all id fields to ObjectId
queryToDoc :: RecordDefinition -> Text -> Either String Document
queryToDoc def xs = do
  q <- queryToDoc' xs
  return (getDocument $ convertIds def $ Record q)

-- ^
-- Convert a filter query to a 'Document'
queryToDoc' :: Text -> Either String Document
queryToDoc' xs
  | T.null xs = Right []
  | otherwise = first ("Invalid query: " ++) $ parse xs >>= filterToDoc

-- ^
-- Convert a 'FilterExpr' into a 'Document' that can be used to filter
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

-- ^
-- Convert a MongoDB 'Failure' into an 'ApiError'
dbToApiError :: Failure -> ApiError
dbToApiError (WriteFailure _ msg) = mkApiError status400 msg
dbToApiError (QueryFailure _ msg) = mkApiError status400 msg
dbToApiError err = mkApiError status500 (show err)
