{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Common functionality for Lono persistence modules
module Persistence.Lono.Common where

import Data.Bson
import qualified Data.Map.Strict as Map
import Database.MongoDB (Collection, Index(..))
import Persistence.Common
import Persistence.Facade (validateMulti)
import Types.Common

userDefinition :: RecordDefinition
userDefinition =
  RecordDefinition "users" "users" userIndices $
  Map.fromList
    [ mkReqDef' "email"
    , mkReqDef "disabled" False
    , mkReqDef "admin" False
    , mkOptDef' "imageUrl"
    , mkOptDef' "givenName"
    , mkOptDef' "familyName"
    , mkOptDef' "name"
    , mkOptDef "locale" ("en" :: String)
    ]

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

userCollection :: Collection
userCollection = recordCollection userDefinition

validateUsers
  :: Monad m
  => [Record] -> ApiItemsT [ApiError] m [Record]
validateUsers = validateMulti userDefinition