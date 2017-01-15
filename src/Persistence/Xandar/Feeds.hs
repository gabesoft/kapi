{-# LANGUAGE OverloadedStrings #-}

-- |
-- Feed model schema and indices
module Persistence.Xandar.Feeds where

import Data.Bson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Database.MongoDB
import Persistence.Common
import Types.Common

feedColl :: Collection
feedColl = "feeds"

feedIndices :: [Index]
feedIndices =
  [ Index
    { iColl = feedColl
    , iKey = ["uri" =: (1 :: Int)]
    , iName = "uri_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  ]

feedDefinition :: RecordDefinition
feedDefinition =
  Map.fromList
    [ mkOptDef' "author"
    , mkOptDef' "data"
    , mkOptDef' "date"
    , mkOptDef' "description"
    , mkReqDef  "failedAttempts" (Just 0 :: Maybe Int)
    , mkOptDef' "favicon"
    , mkOptDef' "format"
    , mkOptDef' "generator"
    , mkOptDef' "guid"
    , mkOptDef' "image"
    , mkOptDef' "language"
    , mkOptDef' "lastPostDate"
    , mkOptDef' "lastReadDate"
    , mkOptDef' "lastReadStatus"
    , mkOptDef' "link"
    , mkReqDef  "postCount" (Just 0 :: Maybe Int)
    , mkOptDef' "title"
    , mkReqDef' "uri"
    ]
