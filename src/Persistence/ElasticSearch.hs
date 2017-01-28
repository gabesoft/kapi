{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Functions for interacting with Elasticsearch
module Persistence.ElasticSearch
  ( createIndex
  , countDocuments
  , deleteDocument
  , deleteDocuments
  , deleteIndex
  , getByIds
  , indexDocument
  , indexDocuments
  , mkIdsSearch
  , mkSearch
  , putMapping
  , putMappingFromFile
  , refreshIndex
  , searchDocuments
  ) where

import Control.Applicative
import Control.Monad (join)
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
  => a -> Text -> Text -> Text -> IO (Either EsError Text)
putMapping mapping server index mappingName =
  withBH
    server
    (B.putMapping (IndexName index) (MappingName mappingName) mapping)

-- ^
-- Add a new type from a file to an existing index
putMappingFromFile :: FilePath -> Text -> Text -> Text -> IO (Either EsError Text)
putMappingFromFile file server index mappingName = do
  json <- readFile file
  let obj = A.decode (L.pack json) :: Maybe A.Object
  case obj of
    Just o -> putMapping o server index mappingName
    Nothing ->
      return $
      Left (mkEsError 400 $ "File " ++ file ++ " contains invalid json")

-- ^
-- Index a document
indexDocument :: Record -> Text -> Text -> Text -> Text -> IO (Either EsError Text)
indexDocument record recordId server index mappingName =
  withBH server $
  B.indexDocument
    (IndexName index)
    (MappingName mappingName)
    B.defaultIndexDocumentSettings
    record
    (DocId recordId)

-- ^
-- Index multiple documents
indexDocuments :: [(Record, Text)] -> Text -> Text -> Text -> IO (Either EsError Text)
indexDocuments items server index mappingName = withBH server (B.bulk stream)
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
deleteDocument recordId server index mappingName =
  withBH server $
  B.deleteDocument (IndexName index) (MappingName mappingName) (DocId recordId)

-- ^
-- Delete multiple documents
deleteDocuments :: [Text] -> Text -> Text -> Text -> IO (Either EsError Text)
deleteDocuments recordIds server index mappingName =
  withBH server (B.bulk stream)
  where
    op recordId =
      BulkDelete (IndexName index) (MappingName mappingName) (DocId recordId)
    stream = V.fromList (op <$> recordIds)

-- ^
-- Get documents by id
getByIds :: [Text] -> Text -> Text -> Text -> IO (Either EsError (SearchResult Record))
getByIds ids server index mappingName =
  searchDocuments (mkIdsSearch mappingName ids) server index mappingName

-- ^
-- Return the number of documents matching a query
countDocuments :: Search -> Text -> Text -> Text -> IO (Either EsError Int)
countDocuments search server index mappingName = do
  res <- searchDocuments (search { size = Size 0 }) server index mappingName
  return $ B.hitsTotal . searchHits <$> res

-- ^
-- Search documents given a 'Search' object
searchDocuments :: Search -> Text -> Text -> Text -> IO (Either EsError (SearchResult Record))
searchDocuments search server index mappingName = do
  body <-
    withBH server $ B.searchByType (IndexName index) (MappingName mappingName) search
  return $ body >>= toResult
  where
    toErr msg = mkEsError 500 ("Failed to decode search results " ++ msg)
    toResult body = first toErr (A.eitherDecode $ textToBytes body)

-- ^
-- Create an 'EsError' with the given HTTP code and message
mkEsError :: Int -> String -> EsError
mkEsError code = EsError code . T.pack

-- ^
-- Create a search object for finding records by id
mkIdsSearch :: Text -> [Text] -> Search
mkIdsSearch mappingName ids = B.mkSearch (Just query) Nothing
  where
    query = IdsQuery (MappingName mappingName) (DocId <$> ids)

-- ^
-- Create a search object
mkSearch :: Maybe FilterExpr -> [Text] -> [Text] -> RecordStart -> ResultLimit -> Either EsError Search
mkSearch expr sort fields' start limit = first mkError $ liftA search query
  where
    query = sequence (exprToQuery <$> expr)
    sort' = mToMaybe $ exprToSort <$> catMaybes (mkSortExpr <$> sort)
    search q = mkSearch' q Nothing sort' (FieldName <$> fields') start limit
    mkError = mkEsError 400

mkSearch' :: Maybe Query -> Maybe Filter -> Maybe Sort -> [FieldName] -> Int -> Int -> Search
mkSearch' query filter' sort fields' start limit =
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
    (mToMaybe fields')
    Nothing

exprToQuery :: FilterExpr -> Either String Query
exprToQuery = toQuery
  where
    toQuery (FilterRelOp Equal col val) = mkEqQuery col val
    toQuery (FilterRelOp In col val) = mkInQuery col val
    toQuery (FilterRelOp Contains col val) = mkMatchQuery' col val
    toQuery (FilterRelOp NotEqual col val) =
      mkNotQuery $ exprToQuery (FilterRelOp Equal col val)
    toQuery (FilterRelOp NotIn col val) =
      mkNotQuery $ exprToQuery (FilterRelOp In col val)
    toQuery (FilterRelOp NotContains col val) =
      mkNotQuery $ exprToQuery (FilterRelOp Contains col val)
    toQuery (FilterRelOp op col val) = mkRangeQuery' col val op
    toQuery (FilterBoolOp Or e1 e2) =
      mkOrQuery (exprToQuery e1) (exprToQuery e2)
    toQuery (FilterBoolOp And e1 e2) =
      mkAndQuery (exprToQuery e1) (exprToQuery e2)
    mkBoost 1 = Nothing
    mkBoost n = Just $ Boost n
    mkBoost' n = fromMaybe (Boost 1) (mkBoost n)
    mkNotQuery = liftA (\q -> QueryBoolQuery $ B.mkBoolQuery [] [q] [])
    mkCompositeQuery mkQuery q1 q2 = QueryBoolQuery $ mkQuery q1 q2
    mkAndQuery = compose (\q1 q2 -> B.mkBoolQuery [q1, q2] [] [])
    mkOrQuery = compose (\q1 q2 -> B.mkBoolQuery [] [] [q1, q2])
    mkMatchQuery' (ColumnName c x) (TermStr v) =
      Right . QueryMatchQuery $ (mkMatchQuery c v $ hasSpace v) (mkBoost x)
    mkMatchQuery' _ t = Left $ "Unexpected " ++ show t ++ ". Expected a string."
    mkInQuery _ (TermList []) = Left "Unexpected empty list."
    mkInQuery (ColumnName c _) (TermList (y:ys)) =
      Right $ TermsQuery c (termToText <$> y :| ys)
    mkInQuery _ t = Left $ "Unexpected " ++ show t ++ ". Expected a list."
    mkEqQuery (ColumnName c x) (TermInt v) =
      Right $ TermQuery (Term c (anyToText v)) (mkBoost x)
    mkEqQuery (ColumnName c x) (TermFloat v) =
      Right $ TermQuery (Term c (anyToText v)) (mkBoost x)
    mkEqQuery (ColumnName c x) (TermBool v) =
      Right $ TermQuery (Term c (boolToText v)) (mkBoost x)
    mkEqQuery (ColumnName c x) (TermStr v) =
      Right $ TermQuery (Term c v) (mkBoost x)
    mkEqQuery (ColumnName c x) (TermDate v) =
      Right $ TermQuery (Term c (anyToText v)) (mkBoost x)
    mkEqQuery _ TermNull = Left "Null query not yet supported."
    mkEqQuery _ t =
      Left $ "Unexpected " ++ show t ++ ". Expected a single term."
    mkRangeQuery' col val op = QueryRangeQuery <$> mkRangeQuery col val op
    mkRangeQuery (ColumnName c x) (TermInt v) op =
      Right $
      RangeQuery (FieldName c) (mkRangeDouble op (fromIntegral v)) (mkBoost' x)
    mkRangeQuery (ColumnName c x) (TermFloat v) op =
      Right $ RangeQuery (FieldName c) (mkRangeDouble op v) (mkBoost' x)
    mkRangeQuery (ColumnName c x) (TermDate v) op =
      Right $ RangeQuery (FieldName c) (mkRangeDate op v) (mkBoost' x)
    mkRangeQuery _ t _ =
      Left $ "Unexpected " ++ show t ++ ". Expected a number or date."
    hasSpace = isJust . T.find isSpace
    compose = liftA2 . mkCompositeQuery

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

boolToText :: Bool -> Text
boolToText True = T.pack "true"
boolToText False = T.pack "false"

anyToText :: Show a => a -> Text
anyToText = T.pack . show

termToText :: FilterTerm -> Text
termToText (TermInt t) = anyToText t
termToText (TermFloat t) = anyToText t
termToText (TermStr t) = t
termToText (TermBool t) = anyToText t
termToText (TermDate t) = anyToText t
termToText TermNull = "null"
termToText t = error $ "cannot convert term " ++ show t ++ "to text"

mToMaybe :: (Eq a, Monoid a) => a -> Maybe a
mToMaybe a
  | a == mempty = Nothing
  | otherwise = Just a
