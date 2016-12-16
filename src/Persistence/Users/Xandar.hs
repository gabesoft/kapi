{-# LANGUAGE OverloadedStrings #-}

-- | Persistence layer for users
module Persistence.Users.Xandar where

import Data.Function ((&))
import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad (fail)
import Data.Aeson
import Data.AesonBson
import Data.Bson
import qualified Data.Map.Strict as Map
import Data.Text (Text, unpack)
import Data.Time
import Database.MongoDB
import Types.Common
import Types.Xandar

data FieldDefinition a = FieldDefinition
  { _fieldToField :: a -> Field
  , _fieldLabel :: Label
  , _fieldRequired :: Bool
  , _fieldDefault :: Maybe a
  , _fieldValue :: Maybe a
  }

data FieldValue
  = FieldValueId ObjectId
  | FieldValueBool Bool
  | FieldValueInt Int
  | FieldValueText Text
  | FieldValueTime UTCTime

data FieldDefinitionSum
  = FieldDefinitionId (FieldDefinition ObjectId)
  | FieldDefinitionBool (FieldDefinition Bool)
  | FieldDefinitionInt (FieldDefinition Int)
  | FieldDefinitionText (FieldDefinition Text)
  | FieldDefinitionTime (FieldDefinition UTCTime)

data RecordDefinition =
  RecordDefinition (Map.Map Label FieldDefinitionSum)

indices :: [Index]
indices = []

getValue :: Val a => Text -> Record -> Maybe a
getValue name r = r ^=. name

setValue :: Val a => Text -> Record -> a -> Record
setValue name r v = r & name .=~ v

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

data ValidationResult
  = RecordValid
  | ValidationErrors [String]

-- |
-- Validate a user
validate :: User -> ValidationResult
validate = undefined

-- TODO ensure `read oid` will not fail
selectById
  :: Select a
  => Collection -> String -> a
selectById coll oid = select ["_id" =: ObjId (read oid)] coll
