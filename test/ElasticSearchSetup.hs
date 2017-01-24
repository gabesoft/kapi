{-# LANGUAGE OverloadedStrings #-}

-- ^
-- GHCI setup for elastic-search
module Test.ElasticSearchSetup where

import Data.Text (Text)
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

fromRight
  :: Show s
  => Either s a -> a
fromRight (Right x) = x
fromRight (Left y) = error (show y)
