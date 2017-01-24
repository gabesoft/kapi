{-# LANGUAGE OverloadedStrings #-}

-- ^
-- GHCI setup for elastic-search
module Test.ElasticSearchSetup where

import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.Bloodhound as B
import Parsers.Filter
import Persistence.ElasticSearch
import Types.Common

eServer :: Text
eServer = "http://localhost:9200"

eIndex :: Text
eIndex = "kapi-xandar"

eMapping :: Text
eMapping = "post"

src :: Text -> RecordStart -> ResultLimit -> B.Search
src input = mkSearch (fromRight $ parse input) []

fromRight :: Show s => Either s a -> a
fromRight (Right x) = x
fromRight (Left y) = error (show y)

withBH :: (Text -> Text -> Text -> t) -> t
withBH f = f eServer eIndex eMapping