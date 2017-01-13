{-# LANGUAGE OverloadedStrings #-}

-- |
-- Post model schema and indices
module Persistence.Xandar.Posts where

import Data.Bson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Database.MongoDB
import Persistence.Common
import Types.Common

postColl :: Collection
postColl = "posts"

postIndices :: [Index]
postIndices =
  [ Index
    { iColl = postColl
    , iKey = ["guid" =: (1 :: Int)]
    , iName = "guid_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  , Index
    { iColl = postColl
    , iKey = ["link" =: (1 :: Int)]
    , iName = "link_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  ]

postDefinition :: RecordDefinition
postDefinition =
  Map.fromList
    [ mkOptDef' "author"
    , mkOptDef' "comments"
    , mkOptDef' "date"
    , mkOptDef' "description"
    , mkReqDef' "feedId"
    , mkReqDef' "guid"
    , mkOptDef' "image"
    , mkOptDef' "inlineStatus"
    , mkReqDef' "link"
    , mkOptDef' "pubdate"
    , mkOptDef' "source"
    , mkOptDef' "summary"
    , mkOptDef' "title"
    ]
