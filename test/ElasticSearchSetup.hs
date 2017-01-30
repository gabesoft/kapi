{-# LANGUAGE OverloadedStrings #-}

-- ^
-- GHCI setup for elastic-search
module Test.ElasticSearchSetup where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.Bloodhound as B
import Parsers.Filter
import Persistence.ElasticSearch
import Persistence.Xandar.UserPosts
import Types.Common

eServer :: Text
eServer = "http://localhost:9200"

eIndex :: Text
eIndex = "kapi-xandar"

eMapping :: Text
eMapping = "post"

src :: Text -> RecordStart -> ResultLimit -> B.Search
src input = src' input [] []

src' :: Text -> [Text] -> [Text] -> RecordStart -> ResultLimit -> B.Search
src' input sort fields start limit =
  fromRight $ mkSearch (Just $ fromRight $ parse input) sort fields start limit

fromRight
  :: Show s
  => Either s a -> a
fromRight (Right x) = x
fromRight (Left y) = error (show y)

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
    addId post = (post, fromJust $ mkUserPostId post)
