{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Functionality for interacting with MongoDB
module Persistence.MongoDB
  ( dbAddIndex
  , dbCount
  , dbDeleteByQuery
  , dbDeleteById
  , dbFind
  , dbGetById
  , dbInsert
  , dbToApiError
  , dbUpdate
  , documentToRecord
  , idQuery
  , idsQuery
  , idsQuery'
  , mkInDocument
  , mkIncludeField
  , mkIncludeFields
  , mkOutRecord
  , mkPipe
  , mkSortField
  , mkSortFields
  , queryToDoc
  , recordToDocument
  , validateRecordHasId
  ) where

import Control.Applicative ((<|>))
import Control.Exception.Lifted (handleJust)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Bifunctor
import Data.Bson
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime(..), parseTimeM, defaultTimeLocale, rfc822DateFormat)
import Data.Time.ISO8601
import Database.MongoDB
import Network.Socket (HostName, PortNumber)
import Parsers.Filter (parse)
import Persistence.Common
import Text.Read (readMaybe)
import Types.Common
import Util.Constants
import Util.Error

-- ^
-- Create a database connection
-- >>> pipe <- mkPipe "127.0.0.1" 27017
-- >>> close pipe
mkPipe :: (HostName, PortNumber) -> IO Pipe
mkPipe (addr, port) = connect (Host addr $ PortNumber port)

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
-- Ensure an index exists on the specified database
dbEnsureIndex :: MonadIO m => Index -> Database -> Pipe -> m ()
dbEnsureIndex idx = dbAccess (ensureIndex idx)

-- ^
-- Ensure a list of indices exist on the specified database
dbEnsureIndices :: MonadIO m => [Index] -> Database -> Pipe -> m ()
dbEnsureIndices indices dbName pipe =
  mapM_ (\idx -> dbEnsureIndex idx dbName pipe) indices

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
  do dbEnsureIndices (recordIndices def) dbName pipe
     input <- mkInDocument def True record
     saved <- dbAccess (action input) dbName pipe
     let maybeId = objIdToRecId saved
     return $ maybe (Left writeError) Right maybeId
  where
    action = insert (recordCollection def)
    writeError = WriteFailure 0 0 "Unexpected missing document"

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
  do dbEnsureIndices (recordIndices def) dbName pipe
     doc <- mkInDocument def False input
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
dbFind def query sort' fields skip' limit' dbName pipe =
  dbAction $
  do docs <- dbAccess action dbName pipe
     return $ Right (mkOutRecord def <$> docs)
  where
    collName = recordCollection def
    action =
      find
        (select query collName)
        { sort = sort'}
        { project = fields}
        { skip = fromIntegral skip'}
        { limit = fromIntegral limit'} >>= rest

-- ^
-- Count the number of records matching a query
dbCount
  :: (MonadBaseControl IO m, MonadIO m)
  => RecordDefinition -> [Field] -> Database -> Pipe -> m (Either Failure Int)
dbCount def query dbName pipe = dbAction $ Right <$> dbAccess action dbName pipe
  where
    collName = recordCollection def
    action = count (select query collName)

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
    query = idQuery recId
    action = findOne (select query collName)

-- ^
-- Delete all records matching a query
dbDeleteByQuery
  :: (MonadBaseControl IO m, MonadIO m)
  => RecordDefinition -> [Field] -> Database -> Pipe -> m (Either Failure ())
dbDeleteByQuery def query dbName pipe =
    dbAction $ Right <$> dbAccess action dbName pipe
    where
      colName = recordCollection def
      action = delete (select query colName)

-- ^
-- Delete a record by id
dbDeleteById
  :: (MonadBaseControl IO m, MonadIO m)
  => RecordDefinition -> RecordId -> Database -> Pipe -> m (Either Failure ())
dbDeleteById def recId dbName pipe =
  dbAction $ Right <$> dbAccess action dbName pipe
  where
    colName = recordCollection def
    query = idQuery recId
    action = delete (select query colName)

-- ^
-- Perform a database action that could result in a 'Failure'
dbAction :: (MonadIO m, MonadBaseControl IO m) => m (Either Failure a) -> m (Either Failure a)
dbAction = handleJust Just (return . Left)

-- ^
-- Create a record ready to be returned from a query action
mkOutRecord :: RecordDefinition -> Document -> Record
mkOutRecord def = excludeFields ["__v"] . documentToRecord def

-- ^
-- Create a document ready to be saved or updated
mkInDocument :: MonadIO m => RecordDefinition -> Bool -> Record -> m Document
mkInDocument def isNew = fmap (recordToDocument def) . setTimestamp isNew

-- ^
-- Convert a BSON 'Document' to a 'Record'
documentToRecord :: RecordDefinition -> Document -> Record
documentToRecord def document =
  foldr mapUTCTimeToDate record (getDateLabels def)
  where
    record = foldr mapIdToRecId (Record document) (idLabel : getIdLabels def)

-- ^
-- Convert a 'Record' to a BSON 'Document'
recordToDocument :: RecordDefinition -> Record -> Document
recordToDocument def record = getDocument datesMapped
  where
    idsMapped = foldr mapIdToObjId record $ idLabel : getIdLabels def
    datesMapped = foldr mapDateToUTCTime idsMapped $ getDateLabels def

-- ^
-- Get the labels of all fields of type 'ObjectId' in a 'RecordDefinition'
getIdLabels :: RecordDefinition -> [Label]
getIdLabels = Map.keys . Map.filter isObjectId . recordFields

-- ^
-- Get the labels of all date fields in a 'RecordDefinition'
getDateLabels :: RecordDefinition -> [Label]
getDateLabels = Map.keys . Map.filter isJsDate . recordFields

-- ^
-- Get a query for finding one record by id
idQuery :: RecordId -> [Field]
idQuery recId = [idLabel =: recIdToObjId recId]

-- ^
-- Get a query for finding multiple records by id
idsQuery :: [RecordId] -> [Field]
idsQuery = idsQuery' idLabel

-- ^
-- Get a query for finding multiple records by id
idsQuery' :: Label -> [RecordId] -> [Field]
idsQuery' name recIds = [name =: ("$in" =: recIdToObjId <$> recIds)]

-- ^
-- Convert a record id to an object id value
recIdToObjId :: RecordId -> Maybe Value
recIdToObjId rid = ObjId <$> readMaybe (T.unpack rid)

-- ^
-- Parse an RFC822 date into a 'UTCTime' object
parseRFC822 :: String -> Maybe UTCTime
parseRFC822 = parseTimeM True defaultTimeLocale rfc822DateFormat

-- ^
-- Parse an Javascript date into a 'UTCTime' object
parseJsDate :: String -> Maybe UTCTime
parseJsDate input = parseISO8601 input <|> parseRFC822 input

-- ^
-- Convert a Javascript date into 'Text'
formatJsDate :: UTCTime -> Maybe Text
formatJsDate = Just . T.pack . formatISO8601Javascript

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
-- Convert a date field in string format into a 'UTCTime' format
mapDateToUTCTime :: Label -> Record -> Record
mapDateToUTCTime name = modField name (>>= parseJsDate)

mapUTCTimeToDate :: Label -> Record -> Record
mapUTCTimeToDate name = modField name (>>= formatJsDate)

-- ^
-- Validate that a record has a valid id field
validateRecordHasId :: Record -> (Record, ValidationResult)
validateRecordHasId r = (r, ValidationErrors $ catMaybes [valField])
  where
    valField = validateField False idDefinition (mapIdToObjId idLabel r) idLabel

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
queryToDoc def xs = convertIds def <$> queryToDoc' xs

-- ^
-- Convert a filter query to a 'Document'
queryToDoc' :: Text -> Either String Document
queryToDoc' xs
  | T.null xs = Right []
  | otherwise = first ("Invalid query: " ++) $ parse xs >>= filterToDoc

-- ^
-- Convert all id values to ObjectId
convertIds :: RecordDefinition -> Document -> Document
convertIds def doc = foldr mapId doc (idLabel : getIdLabels def)
  where
    mapId name acc = maybe acc (processField acc) (getField name $ Record doc)
    processField d f = getDocument $ setField (process f) (Record d)
    process (k := v) = k := mapValue v
    mapValue (String t) = fromMaybe (String t) (recIdToObjId t)
    mapValue (Array xs) = Array (mapValue <$> xs)
    mapValue (Doc d) = Doc (process <$> d)
    mapValue v = v

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
dbToApiError (WriteFailure _ _ msg) = mk400Err' msg
dbToApiError (QueryFailure _ msg) = mk400Err' msg
dbToApiError (CompoundFailure [WriteFailure _ _ msg]) = mk400Err' msg
dbToApiError (CompoundFailure [QueryFailure _ msg]) = mk400Err' msg
dbToApiError err = mk500Err' (show err)
