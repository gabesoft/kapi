{-# LANGUAGE OverloadedStrings #-}

-- |
-- User model schema and indices
module Persistence.Xandar.Users where

import Data.Bson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Database.MongoDB
import Persistence.Common
import Types.Common

userColl :: Collection
userColl = "users"

userIndices :: [Index]
userIndices =
  [ Index
    { iColl = userColl
    , iKey = ["email" =: (1 :: Int)]
    , iName = "email_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  ]

userDefinition :: RecordDefinition
userDefinition =
  Map.fromList
    [ mkReqDef' "email"
    , mkReqDef "disabled" (Just False)
    , mkReqDef "admin" (Just False)
    , mkOptDef' "githubAvatar"
    , mkOptDef' "githubUrl"
    , mkOptDef' "githubLogin"
    ]
