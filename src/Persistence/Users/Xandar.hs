{-# LANGUAGE OverloadedStrings #-}

-- | Persistence layer for users
module Persistence.Users.Xandar where

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad (fail)
import Data.Aeson
import Data.AesonBson
import Data.Bson as BSON
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Text (Text, unpack)
import Data.Time
import Database.MongoDB (Index, Select, Collection, select)
import Types.Common
import Types.Xandar

userIndices :: [Index]
userIndices = []

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

getId :: Record -> Maybe Text
getId = getValue "_id"

setId :: Record -> Text -> Record
setId = setValue "_id"

delId :: Record -> Record
delId r = delField r "_id"

getEmail :: Record -> Maybe Text
getEmail = getValue "email"

setEmail :: Record -> Text -> Record
setEmail = setValue "email"

delEmail :: Record -> Record
delEmail r = delField r "email"

createUser = undefined

updateUser = undefined

getUser = undefined

getUsers = undefined

deleteUser = undefined

-- |
-- Validate a user
-- validate :: User -> ValidationResult
-- validate = undefined
-- TODO ensure `read oid` will not fail
selectById
  :: Select a
  => Collection -> String -> a
selectById coll oid = select ["_id" =: ObjId (read oid)] coll

-- |
-- Sample records. TO BE REMOVED.
-- "_id" =: (read "584e58195984185eb8000005" :: ObjectId)
u1 =
  Record
    [ "_id" =: ("584e58195984185eb8000005" :: String)
    , "email" =: ("blue@leaf.com" :: String)
    , "githubUrl" =: ("https://github.com/api/users/mrblue" :: String)
    ]

u2 =
  Record
    [ "_id" =: ("584e58195984185eb8000006" :: String)
    , "email" =: ("green@leaf.com" :: String)
    , "githubUrl" =: ("https://github.com/api/users/mrgreen" :: String)
    ]
