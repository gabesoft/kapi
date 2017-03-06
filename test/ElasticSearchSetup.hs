{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- GHCI setup for elastic-search
module Test.ElasticSearchSetup where

import Control.Applicative
import qualified Data.Aeson as A
import Data.Bson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List.NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Database.Bloodhound as B
import Network.HTTP.Client
import Parsers.Filter
import Persistence.Common
import Persistence.ElasticSearch
import Persistence.Xandar.UserPosts
import TestHelper
import Types.Common
import Util.Constants

mongoDbHost :: String
mongoDbHost = "127.0.0.1"

mongoDbPort :: Int
mongoDbPort = 27017

mongoDbName :: Text
mongoDbName = "kapi-xandar"

eServer :: Text
eServer = "http://localhost:9200"

eIndex :: Text
eIndex = "kapi-xandar"

eMapping :: Text
eMapping = "post"

sampleConf :: ApiConfig
sampleConf =
  ApiConfig
  { apiPort = 8001
  , appName = Just "xandar"
  , mongoHost = mongoDbHost
  , mongoPort = fromIntegral mongoDbPort
  , mongoDbs = Map.fromList [("xandar", mongoDbName)]
  , esServer = eServer
  , esIndices = Map.fromList [("xandar", eIndex)]
  }

src :: Text -> RecordStart -> ResultLimit -> B.Search
src input = src' input [] []

src' :: Text -> [Text] -> [Text] -> RecordStart -> ResultLimit -> B.Search
src' input sort' fields start limit =
  fromRight $ mkSearch (Just $ fromRight $ parse input) sort' fields start limit

withBH :: (Text -> Text -> Text -> t) -> t
withBH f = f eServer eIndex eMapping

readPosts :: IO [Record]
readPosts = do
  text <- readFile "./test/Data/user-posts.json"
  let records = A.decode (LBS.pack text) :: Maybe [Record]
  return (fromJust records)

readItems :: IO [(Record, Text)]
readItems = do
  posts <- readPosts
  return (addId <$> posts)
  where
    addId :: Record -> (Record, Text)
    addId post =
      ( post
      , fromJust $
        liftA2
          mkUserPostId
          (getValue "subscriptionId" post)
          (getValue "postId" post))

scanSearch :: Text -> IO [B.Hit Record]
scanSearch input = B.withBH defaultManagerSettings (B.Server eServer) doSearch
  where
    search = src input 0 1048576
    search' = search {B.fields = Just [B.FieldName idLabel]}
    doSearch =
      B.scanSearch (B.IndexName eIndex) (B.MappingName eMapping) search'

countsSearch :: B.Search -> IO (Either B.EsError (Map.Map Text Int))
countsSearch search = do
  result <- searchDocuments search eMapping eServer eIndex
  return (extractCounts "unreadCountsPerSub" <$> result)

extractCounts :: Text -> B.SearchResult a -> Map.Map Text Int
extractCounts bucketKey results =
  fromMaybe Map.empty $ toBucketMap <$> toBuckets
  where
    toBuckets = B.aggregations results >>= B.toTerms bucketKey
    extractVal (B.TextValue v) = v
    extractVal v = error $ "Expected TextValue " <> show v
    extractTerm t = (extractVal (B.termKey t), B.termsDocCount t)
    toBucketMap = Map.fromList . fmap extractTerm . B.buckets

subIds :: [Text]
subIds =
  [ "56e60f7d1432afe539337b7e"
  , "56e5c9cb1432afe53933789e"
  , "56d7e167c788cb1d6eb935f3"
  , "56e5cc021432afe539337994"
  , "56d7de19c788cb1d6eb91b22"
  , "56d7e1b2c788cb1d6eb9396a"
  , "56d7e064c788cb1d6eb92ea2"
  , "5792e720ed521f1f1704877f"
  ]

printSearch
  :: B.ToJSON a
  => a -> IO ()
printSearch = putStrLn . LBS.unpack . A.encode

subCountsSearch :: [Text] -> B.Search
subCountsSearch ids =
  B.mkAggregateSearch (Just $ B.QueryBoolQuery boolQuery) termsAgg
  where
    boolQuery = B.mkBoolQuery [readQuery, idsQuery] [] []
    readQuery = B.TermQuery (B.Term "read" "false") Nothing
    idsQuery = B.TermsQuery "subscriptionId" (fromList ids)
    termsAgg = B.mkAggregations "unreadCountsPerSub" agg
    agg = B.TermsAgg $ B.mkTermsAggregation "subscriptionId"

input1 :: RecordData Field
input1 =
  Record
    [ mkStrField "postId" "56d7d88bc788cb1d6eb9199d"
    , mkStrField "subscriptionId" "56d7df90c788cb1d6eb92789"
    , mkStrField "title" "Haskell  Reddit"
    , mkBoolField "read" True
    ]
