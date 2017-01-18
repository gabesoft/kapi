-- |
-- Functions for interacting with Elasticsearch
module Persistence.ElasticSearch
  ( createIndex
  , refreshIndex
  , putMapping
  , putMappingFromFile
  , indexDocument
  ) where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Database.Bloodhound
       (IndexName(..), Server(..), EsError(..), Reply(..),
        MappingName(..), DocId(..), BH)
import qualified Database.Bloodhound as B
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Types.Common

-- |
-- Create an index named @indexName@ on @server@
createIndex :: Text -> Text -> IO (Either EsError Text)
createIndex server index =
  withBH server (B.createIndex B.defaultIndexSettings $ IndexName index)

-- |
-- Refresh an index. Should be called after writing to an index.
refreshIndex :: Text -> Text -> IO (Either EsError Text)
refreshIndex server index = withBH server (B.refreshIndex $ IndexName index)

-- |
-- Add a new type to an existing index
putMapping
  :: ToJSON a
  => Text -> Text -> Text -> a -> IO (Either EsError Text)
putMapping server index mappingName mapping =
  withBH
    server
    (B.putMapping (IndexName index) (MappingName mappingName) mapping)

-- |
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
    Just o -> putMapping server index mappingName obj
    Nothing ->
      return $
      Left (EsError 400 $ T.pack $ "File " ++ file ++ " contains invalid json")

-- |
-- Index a document
indexDocument :: Text
              -> Text
              -> Text
              -> Record
              -> Text
              -> IO (Either EsError Text)
indexDocument server index mappingName record recordId =
  withBH
    server
    (B.indexDocument
       (IndexName index)
       (MappingName mappingName)
       B.defaultIndexDocumentSettings
       record
       (DocId recordId))

withBH :: Text -> BH IO Reply -> IO (Either EsError Text)
withBH server action = do
  reply <- B.withBH defaultManagerSettings (Server server) action
  return $ fromReply reply
  where
    body = decodeUtf8 . L.toStrict . responseBody
    code = statusCode . responseStatus
    ok s = (s == status200) || (s == status201)
    fromReply reply
      | ok (responseStatus reply) = Right (body reply)
      | otherwise = Left $ EsError (code reply) (body reply)
