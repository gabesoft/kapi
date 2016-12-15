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

userDefinition :: RecordDefinition
userDefinition =
  RecordDefinition $
  Map.fromList
    [ mkDefinitionId "_id" False Nothing
    , mkDefinitionText "email" True Nothing
    , mkDefinitionBool "admin" False (Just False)
    , mkDefinitionBool "disabled" False (Just False)
    , mkDefinitionTime "createdAt" False Nothing
    , mkDefinitionTime "updatedAt" False Nothing
    , mkDefinitionText "githubAvatar" False Nothing
    , mkDefinitionText "githubUrl" False Nothing
    , mkDefinitionText "githubLogin" False Nothing
    ]

-- TODO: create typed lenses
_id f v = f ("_id" :: Text) (read (unpack v) :: ObjectId)

instance ToJSON RecordDefinition where
  toJSON (RecordDefinition record) = object (toField <$> Map.elems record)
    where
      toField (FieldDefinitionId field) =
        _fieldLabel field .= (show <$> _fieldValue field :: Maybe String)
      toField (FieldDefinitionBool field) =
        _fieldLabel field .= (_fieldValue field <|> _fieldDefault field)
      toField (FieldDefinitionInt field) =
        _fieldLabel field .= (_fieldValue field <|> _fieldDefault field)
      toField (FieldDefinitionText field) =
        _fieldLabel field .= (_fieldValue field <|> _fieldDefault field)
      toField (FieldDefinitionTime field) =
        _fieldLabel field .= (_fieldValue field <|> _fieldDefault field)

populateUser values =
  RecordDefinition $ Map.fromList $ setValue values <$> fields
  where
    definition (RecordDefinition d) = d
    fields = Map.toList (definition userDefinition)

u3 =
  populateUser $
  Map.fromList
    [ ("_id", FieldValueId (read "584e58195984185eb8000005"))
    , ("email", FieldValueText "blue@swan.com")
    , ("githubUrl", FieldValueText "/api/users/blue")
    ]

setValue values (label, FieldDefinitionId def) =
  case Map.lookup label values of
    Nothing -> (label, FieldDefinitionId def)
    Just (FieldValueId v) ->
      (label, FieldDefinitionId $ def {_fieldValue = Just v})
setValue values (label, FieldDefinitionBool def) =
  case Map.lookup label values of
    Nothing -> (label, FieldDefinitionBool def)
    Just (FieldValueBool v) ->
      (label, FieldDefinitionBool $ def {_fieldValue = Just v})
setValue values (label, FieldDefinitionInt def) =
  case Map.lookup label values of
    Nothing -> (label, FieldDefinitionInt def)
    Just (FieldValueInt v) ->
      (label, FieldDefinitionInt $ def {_fieldValue = Just v})
setValue values (label, FieldDefinitionText def) =
  case Map.lookup label values of
    Nothing -> (label, FieldDefinitionText def)
    Just (FieldValueText v) ->
      (label, FieldDefinitionText $ def {_fieldValue = Just v})
setValue values (label, FieldDefinitionTime def) =
  case Map.lookup label values of
    Nothing -> (label, FieldDefinitionTime def)
    Just (FieldValueTime v) ->
      (label, FieldDefinitionTime $ def {_fieldValue = Just v})

mkDefinition
  :: Val v
  => (FieldDefinition v -> t) -> Text -> Bool -> Maybe v -> (Text, t)
mkDefinition ctor label required defval =
  (label, ctor $ FieldDefinition (label =:) label required defval Nothing)

mkDefinitionId = mkDefinition FieldDefinitionId

mkDefinitionBool = mkDefinition FieldDefinitionBool

mkDefinitionText = mkDefinition FieldDefinitionText

mkDefinitionTime = mkDefinition FieldDefinitionTime

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
