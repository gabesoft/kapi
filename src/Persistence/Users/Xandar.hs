{-# LANGUAGE OverloadedStrings #-}

-- | Persistence layer for users
module Persistence.Users.Xandar where

import Data.Text (Text)
import Data.Bson
import qualified Data.Map.Strict as Map
import Database.MongoDB
import Types.Common

dbName :: Database
dbName = "kapi-xandar"

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
    [ mkReqDef "email"
    , mkDef "admin" False (Just False)
    , mkDef "disabled" True (Just False)
    , mkOptDef "githubAvatar"
    , mkOptDef "githubUrl"
    , mkOptDef "githubLogin"
    ]

getId :: Record -> Maybe RecordId
getId = getValue "_id"

setId :: Record -> RecordId -> Record
setId = setValue "_id"

delId :: Record -> Record
delId r = delField r "_id"

getEmail :: Record -> Maybe Text
getEmail = getValue "email"

setEmail :: Record -> Text -> Record
setEmail = setValue "email"

delEmail :: Record -> Record
delEmail r = delField r "email"

-- |
-- Sample records. TO BE REMOVED.
-- "_id" =: (read "584e58195984185eb8000005" :: ObjectId)
u1 :: Record
u1 =
  Record
    [ "_id" =: ("584e58195984185eb8000005" :: String)
    , "email" =: ("blue@leaf.com" :: String)
    , "githubUrl" =: ("https://github.com/api/users/mrblue" :: String)
    ]

u2 :: Record
u2 =
  Record
    [ "_id" =: ("584e58195984185eb8000006" :: String)
    , "email" =: ("green@leaf.com" :: String)
    , "githubUrl" =: ("https://github.com/api/users/mrgreen" :: String)
    ]
