{-# LANGUAGE OverloadedStrings #-}

-- |
-- Feed subscription model schema an indices
module Persistence.Xandar.FeedSubscriptions where

import Data.Bson
import qualified Data.Map.Strict as Map
import Database.MongoDB (Index(..))
import Persistence.Common
import Types.Common

feedSubscriptionIndices :: [Index]
feedSubscriptionIndices =
  [ Index
    { iColl = recordCollection feedSubscriptionDefinition
    , iKey = ["userId" =: (1 :: Int), "feedId" =: (1 :: Int)]
    , iName = "userid_feedid_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  ]

feedSubscriptionDefinition :: RecordDefinition
feedSubscriptionDefinition =
  RecordDefinition "feedsubscriptions" $
  Map.fromList
    [ mkIdDef "userId"
    , mkIdDef "feedId"
    , mkOptDef' "title"
    , mkOptDef' "notes"
    , mkOptDef "tags" (Just [] :: Maybe [String])
    , mkReqDef "disabled" (Just False :: Maybe Bool)
    , mkOptDef "unreadCount" (Just 0 :: Maybe Int)
    ]
