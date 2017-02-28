{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- GHCI setup for elastic-search
module Test.ElasticSearchSetup where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Except
import qualified Data.Aeson as A
import Data.Bson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.Bloodhound as B
import Database.MongoDB (Pipe)
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Parsers.Filter
import Persistence.Common
import Persistence.ElasticSearch
import Persistence.Facade (dbPipe)
import Persistence.Xandar.Common
import Persistence.Xandar.UserPosts
import TestHelper
import Types.Common

mongoDbHost = "127.0.0.1"
mongoDbPort = 27017
mongoDbName = "kapi-xandar"

eServer :: Text
eServer = "http://localhost:9200"

eIndex :: Text
eIndex = "kapi-xandar"

eMapping :: Text
eMapping = "post"

sampleConf =
  ApiConfig
  { apiPort = 8001
  , mongoHost = mongoDbHost
  , mongoPort = mongoDbPort
  , mongoDbs = Map.fromList [("xandar", mongoDbName)]
  , esServer = eServer
  , esIndices = Map.fromList [("xandar", eIndex)]
  }

src :: Text -> RecordStart -> ResultLimit -> B.Search
src input = src' input [] []

src' :: Text -> [Text] -> [Text] -> RecordStart -> ResultLimit -> B.Search
src' input sort fields start limit =
  fromRight $ mkSearch (Just $ fromRight $ parse input) sort fields start limit

withBH :: (Text -> Text -> Text -> t) -> t
withBH f = f eServer eIndex eMapping

readPosts = do
  text <- readFile "./test/Data/user-posts.json"
  let records = A.decode (LBS.pack text) :: Maybe [Record]
  return (fromJust records)

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

scanSearch input = B.withBH defaultManagerSettings (B.Server eServer) scanSearch
  where
    search = src input 0 1048576
    search' = search {B.fields = Just [B.FieldName idLabel]}
    scanSearch
      :: (B.MonadBH m, MonadThrow m)
      => m [B.Hit Record]
    scanSearch =
      B.scanSearch (B.IndexName eIndex) (B.MappingName eMapping) search'

input1 :: RecordData Field
input1 =
  Record
    [ mkStrField "postId" "56d7d88bc788cb1d6eb9199d"
    , mkStrField "subscriptionId" "56d7df90c788cb1d6eb92789"
    , mkStrField "title" "Haskell  Reddit"
    , mkBoolField "read" True
    ]

getDbData :: [Record] -> ExceptT ApiError IO ([Record], [Record], [Text], [Text])
getDbData input = do
  pipe <- dbPipe sampleConf
  let subIds = vals "subscriptionId" input
  let postIds = vals "postId" input
  subs <- runDb (getSubsById subIds) mongoDbName pipe
  posts <- runDb (getPostsById postIds) mongoDbName pipe
  return (subs, posts, subIds, postIds)
  where
    vals label = catMaybes . fmap (getValue label)