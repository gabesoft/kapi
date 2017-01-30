{-# LANGUAGE OverloadedStrings #-}

-- ^
-- User model schema and indices
module Persistence.Xandar.Users where

import Data.Bson
import qualified Data.Map.Strict as Map
import Database.MongoDB (Index(..))
import Persistence.Common
import Types.Common

userIndices :: [Index]
userIndices =
  [ Index
    { iColl = recordCollection userDefinition
    , iKey = ["email" =: (1 :: Int)]
    , iName = "email_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  ]

userDefinition :: RecordDefinition
userDefinition =
  RecordDefinition "users" $
  Map.fromList
    [ mkReqDef' "email"
    , mkReqDef "disabled" (Just False)
    , mkReqDef "admin" (Just False)
    , mkOptDef' "githubAvatar"
    , mkOptDef' "githubUrl"
    , mkOptDef' "githubLogin"
    ]
