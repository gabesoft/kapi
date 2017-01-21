-- ^
-- Functions for interacting with Elasticsearch
module Persistence.ElasticSearch
  ( createIndex
  , deleteDocument
  , deleteDocuments
  , deleteIndex
  , indexDocument
  , indexDocuments
  , mkSearch
  , putMapping
  , putMappingFromFile
  , refreshIndex
  , searchDocuments
  ) where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as A
import Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Vector as V
import Database.Bloodhound
       (IndexName(..), Server(..), EsError(..), Reply, MappingName(..),
        DocId(..), IndexSettings(..), ShardCount(..), ReplicaCount(..),
        BulkOperation(..), Search(..), SearchResult(..), From(..),
        Size(..), SearchType(..), Query(..), Filter(..), FieldName(..),
        SortOrder(..), BH)
import qualified Database.Bloodhound as B
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Persistence.Common
import Types.Common

-- ^
-- Default settings for creating an index on a single node instance
singleNodeIndexSettings :: IndexSettings
singleNodeIndexSettings = IndexSettings (ShardCount 1) (ReplicaCount 0)

-- ^
-- Create an index
createIndex :: Text -> Text -> IO (Either EsError Text)
createIndex server index =
  withBH server (B.createIndex singleNodeIndexSettings $ IndexName index)

-- ^
-- Delete an index
deleteIndex :: Text -> Text -> IO (Either EsError Text)
deleteIndex server index = withBH server (B.deleteIndex $ IndexName index)

-- ^
-- Refresh an index. Should be called after writing to an index.
refreshIndex :: Text -> Text -> IO (Either EsError Text)
refreshIndex server index = withBH server (B.refreshIndex $ IndexName index)

-- ^
-- Add a new type to an existing index
putMapping
  :: ToJSON a
  => Text -> Text -> Text -> a -> IO (Either EsError Text)
putMapping server index mappingName mapping =
  withBH
    server
    (B.putMapping (IndexName index) (MappingName mappingName) mapping)

-- ^
-- Add a new type from a file to an existing index
putMappingFromFile :: Text
                   -> Text
                   -> Text
                   -> FilePath
                   -> IO (Either EsError Text)
putMappingFromFile server index mappingName file = do
  json <- readFile file
  let obj = A.decode (L.pack json) :: Maybe A.Object
  case obj of
    Just o -> putMapping server index mappingName o
    Nothing ->
      return $
      Left (EsError 400 $ T.pack $ "File " ++ file ++ " contains invalid json")

-- ^
-- Index a document
indexDocument :: Text
              -> Text
              -> Text
              -> Record
              -> Text
              -> IO (Either EsError Text)
indexDocument server index mappingName record recordId =
  withBH server $
  B.indexDocument
    (IndexName index)
    (MappingName mappingName)
    B.defaultIndexDocumentSettings
    record
    (DocId recordId)

-- ^
-- Index multiple documents
indexDocuments :: Text
               -> Text
               -> Text
               -> [(Record, Text)]
               -> IO (Either EsError Text)
indexDocuments server index mappingName items = withBH server (B.bulk stream)
  where
    op (record, recordId) =
      BulkIndex
        (IndexName index)
        (MappingName mappingName)
        (DocId recordId)
        (A.toJSON record)
    stream = V.fromList (op <$> items)

-- ^
-- Delete a document
deleteDocument :: Text -> Text -> Text -> Text -> IO (Either EsError Text)
deleteDocument server index mappingName recordId =
  withBH server $
  B.deleteDocument (IndexName index) (MappingName mappingName) (DocId recordId)

-- ^
-- Delete multiple documents
deleteDocuments :: Text -> Text -> Text -> [Text] -> IO (Either EsError Text)
deleteDocuments server index mappingName recordIds =
  withBH server (B.bulk stream)
  where
    op recordId =
      BulkDelete (IndexName index) (MappingName mappingName) (DocId recordId)
    stream = V.fromList (op <$> recordIds)

-- ^
-- Get documents by id
getById server index mappingName ids =
  searchDocuments server index mappingName search
  where
    query = IdsQuery (MappingName mappingName) (DocId <$> ids)
    search = B.mkSearch (Just query) Nothing

-- ^
-- Search documents given a 'Search' object
searchDocuments :: Text
                -> Text
                -> Text
                -> Search
                -> IO (Either EsError (SearchResult Record))
searchDocuments server index mappingName search = do
  body <-
    withBH server $
    B.searchByType (IndexName index) (MappingName mappingName) search
  return $ body >>= toResult
  where
    toErr msg = EsError 500 (T.pack $ "Failed to decode search results " ++ msg)
    toResult body = first toErr (A.eitherDecode $ textToBytes body)

-- ^
-- Create a search object
mkSearch :: Maybe Query -> Maybe Filter -> Int -> Int -> Search
mkSearch query filter start limit =
  Search
    query
    filter
    Nothing
    Nothing
    Nothing
    False
    (From start)
    (Size limit)
    SearchTypeDfsQueryThenFetch
    Nothing
    Nothing

withBH :: Text -> BH IO Reply -> IO (Either EsError Text)
withBH server action = do
  reply <- B.withBH defaultManagerSettings (Server server) action
  return $ fromReply reply
  where
    body = bytesToText . responseBody
    code = statusCode . responseStatus
    ok s = (s == status200) || (s == status201)
    fromReply reply
      | ok (responseStatus reply) = Right (body reply)
      | otherwise = Left $ EsError (code reply) (body reply)

bytesToText :: L.ByteString -> Text
bytesToText = decodeUtf8 . L.toStrict

textToBytes :: Text -> L.ByteString
textToBytes = L.fromStrict . encodeUtf8

mkSearch' expr sort start limit = undefined
  where
    mkSort (SortExpr n SortAscending) = B.mkSort (FieldName n) Ascending
    mkSort (SortExpr n SortDescending) = B.mkSort (FieldName n) Descending
    sort' = mkSort <$> mkSortExpr sort
