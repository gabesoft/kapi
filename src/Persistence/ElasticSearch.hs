{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Functions for interacting with Elasticsearch
module Persistence.ElasticSearch
  ( createIndex
  , deleteDocument
  , deleteDocuments
  , deleteIndex
  , getById
  , indexDocument
  , indexDocuments
  , mkIdsSearch
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
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime)
import qualified Data.Vector as V
import Database.Bloodhound
       (IndexName(..), Server(..), EsError(..), Reply, MappingName(..),
        DocId(..), IndexSettings(..), ShardCount(..), ReplicaCount(..),
        BulkOperation(..), Search(..), SearchResult(..), From(..),
        Size(..), SearchType(..), Query(..), Filter(..), FieldName(..),
        SortOrder(..), Sort, SortSpec(..), Term(..), Boost(..),
        RangeQuery(..), RangeValue(..), QueryString(..), MatchQuery(..),
        MatchQueryType(..), BH)
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
getById :: Text
        -> Text
        -> Text
        -> [Text]
        -> IO (Either EsError (SearchResult Record))
getById server index mappingName ids =
  searchDocuments server index mappingName (mkIdsSearch mappingName ids)

-- ^
-- Search documents given a 'Search' object
searchDocuments :: Text
                -> Text
                -> Text
                -> Search
                -> IO (Either EsError (SearchResult Record))
searchDocuments server index mappingName search = do
  body <-
    withBH server $ B.searchByType (IndexName index) (MappingName mappingName) search
  return $ body >>= toResult
  where
    toErr msg = EsError 500 (T.pack $ "Failed to decode search results " ++ msg)
    toResult body = first toErr (A.eitherDecode $ textToBytes body)

-- ^
-- Create a search object for finding records by id
mkIdsSearch :: Text -> [Text] -> Search
mkIdsSearch mappingName ids = B.mkSearch (Just query) Nothing
  where
    query = IdsQuery (MappingName mappingName) (DocId <$> ids)

-- ^
-- Create a search object
mkSearch :: FilterExpr -> [Text] -> RecordStart -> ResultLimit -> Search
mkSearch expr sort = mkSearch' query Nothing sort'
  where
    query = exprToQuery expr
    sort' = toMaybe $ exprToSort <$> catMaybes (mkSortExpr <$> sort)
    toMaybe [] = Nothing
    toMaybe xs = Just xs

exprToQuery :: FilterExpr -> Maybe Query
exprToQuery = toQuery
  where
    toQuery (FilterRelOp Equal col val) = mkEqQuery col val
    toQuery (FilterRelOp In col val) = mkInQuery col val
    toQuery (FilterRelOp Contains col val) = mkMatchQuery' col val
    toQuery (FilterRelOp NotEqual col val) =
      mkNotQuery <$> exprToQuery (FilterRelOp Equal col val)
    toQuery (FilterRelOp NotIn col val) =
      mkNotQuery <$> exprToQuery (FilterRelOp In col val)
    toQuery (FilterRelOp NotContains col val) =
      mkNotQuery <$> exprToQuery (FilterRelOp Contains col val)
    toQuery (FilterRelOp op col val) = mkRangeQuery' col val op
    toQuery (FilterBoolOp Or e1 e2) =
      mkOrQuery (exprToQuery e1) (exprToQuery e2)
    toQuery (FilterBoolOp And e1 e2) =
      mkAndQuery (exprToQuery e1) (exprToQuery e2)
    mkBoost 1 = Nothing
    mkBoost n = Just $ Boost n
    mkBoost' n = fromMaybe (Boost 1) (mkBoost n)
    mkNotQuery q = QueryBoolQuery $ B.mkBoolQuery [] [q] []
    mkCompositeQuery mkQuery (Just q1) (Just q2) =
      Just . QueryBoolQuery $ mkQuery q1 q2
    mkCompositeQuery _ (Just q1) _ = Just q1
    mkCompositeQuery _ _ (Just q2) = Just q2
    mkCompositeQuery _ _ _ = Nothing
    mkAndQuery = mkCompositeQuery (\q1 q2 -> B.mkBoolQuery [q1, q2] [] [])
    mkOrQuery = mkCompositeQuery (\q1 q2 -> B.mkBoolQuery [] [] [q1, q2])
    mkMatchQuery' (ColumnName c x) (TermStr v) =
      Just . QueryMatchQuery $ (mkMatchQuery c v $ hasSpace v) (mkBoost x)
    mkMatchQuery' _ t = error $ "invalid term for match query" ++ show t
    mkInQuery _ (TermList []) = Nothing
    mkInQuery (ColumnName c _) (TermList (y:ys)) =
      Just $ TermsQuery c (termToText <$> y :| ys)
    mkInQuery _ t = error $ "unexpected non-list term " ++ show t
    mkEqQuery (ColumnName c x) (TermInt v) =
      Just $ TermQuery (Term c (anyToText v)) (mkBoost x)
    mkEqQuery (ColumnName c x) (TermFloat v) =
      Just $ TermQuery (Term c (anyToText v)) (mkBoost x)
    mkEqQuery (ColumnName c x) (TermBool v) =
      Just $ TermQuery (Term c (anyToText v)) (mkBoost x)
    mkEqQuery (ColumnName c x) (TermStr v) =
      Just $ TermQuery (Term c v) (mkBoost x)
    mkEqQuery (ColumnName c x) (TermDate v) =
      Just $ TermQuery (Term c (anyToText v)) (mkBoost x)
    mkEqQuery _ TermNull = error "null query not supported"
    mkEqQuery _ t = error $ "invalid term for equal " ++ show t
    mkRangeQuery' col val op = Just . QueryRangeQuery $ mkRangeQuery col val op
    mkRangeQuery (ColumnName c x) (TermInt v) op =
      RangeQuery (FieldName c) (mkRangeDouble op (fromIntegral v)) (mkBoost' x)
    mkRangeQuery (ColumnName c x) (TermFloat v) op =
      RangeQuery (FieldName c) (mkRangeDouble op v) (mkBoost' x)
    mkRangeQuery (ColumnName c x) (TermDate v) op =
      RangeQuery (FieldName c) (mkRangeDate op v) (mkBoost' x)
    mkRangeQuery _ t _ = error $ "invalid term for range " ++ show t
    hasSpace = isJust . T.find isSpace

mkRangeDouble :: FilterRelationalOperator -> Double -> RangeValue
mkRangeDouble GreaterThan = RangeDoubleGt . B.GreaterThan
mkRangeDouble GreaterThanOrEqual = RangeDoubleGte . B.GreaterThanEq
mkRangeDouble LessThan = RangeDoubleLt . B.LessThan
mkRangeDouble LessThanOrEqual = RangeDoubleLte . B.LessThanEq
mkRangeDouble op = error $ "invalid range operator " ++ show op

mkRangeDate :: FilterRelationalOperator -> UTCTime -> RangeValue
mkRangeDate GreaterThan = RangeDateGt . B.GreaterThanD
mkRangeDate GreaterThanOrEqual = RangeDateGte . B.GreaterThanEqD
mkRangeDate LessThan = RangeDateLt . B.LessThanD
mkRangeDate LessThanOrEqual = RangeDateLte . B.LessThanEqD
mkRangeDate op = error $ "invalid range operator " ++ show op

mkMatchQuery :: Text -> Text -> Bool -> Maybe Boost -> MatchQuery
mkMatchQuery field query phrase =
  MatchQuery
    (FieldName field)
    (QueryString query)
    B.Or
    B.ZeroTermsNone
    Nothing
    (queryType phrase)
    Nothing
    Nothing
    Nothing
  where
    queryType True = Just MatchPhrase
    queryType False = Nothing

exprToSort :: SortExpr -> SortSpec
exprToSort (SortExpr n SortAscending) =
  DefaultSortSpec $ B.mkSort (FieldName n) Ascending
exprToSort (SortExpr n SortDescending) =
  DefaultSortSpec $ B.mkSort (FieldName n) Descending

mkSearch' :: Maybe Query -> Maybe Filter -> Maybe Sort -> Int -> Int -> Search
mkSearch' query filter' sort start limit =
  Search
    query
    filter'
    sort
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

anyToText
  :: Show a
  => a -> Text
anyToText = T.pack . show

termToText :: FilterTerm -> Text
termToText (TermInt t) = anyToText t
termToText (TermFloat t) = anyToText t
termToText (TermStr t) = t
termToText (TermBool t) = anyToText t
termToText (TermDate t) = anyToText t
termToText TermNull = "null"
termToText t = error $ "cannot convert term " ++ show t ++ "to text"