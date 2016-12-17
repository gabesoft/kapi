{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Common types
module Types.Common where

import Control.Lens (view, over)
import Data.Aeson as AESON
import Data.AesonBson (aesonify, bsonify)
import Data.Bson as BSON
import Data.Function ((&))
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import Data.Time (getCurrentTime, UTCTime(..))
import GHC.Generics

-- |
-- Representation of a record schema
data FieldDefinition = FieldDefinition
  { fieldLabel :: Label
  , fieldRequired :: Bool
  , fieldDefault :: Maybe Field
  } deriving (Eq, Show)

type RecordDefinition = Map.Map Label FieldDefinition

-- |
-- Create a @RecordDefinition@
mkDef
  :: Val a
  => Label -> Bool -> Maybe a -> (Label, FieldDefinition)
mkDef name required def =
  (name, FieldDefinition name required ((name :=) . val <$> def))

-- |
-- Create a @RecordDefinition@ for a required field
mkReqDef :: Label -> (Label, FieldDefinition)
mkReqDef name = mkDef name True (Nothing :: Maybe String)

-- |
-- Create a @RecordDefinition@ for an optional field
mkOptDef :: Label -> (Label, FieldDefinition)
mkOptDef name = mkDef name False (Nothing :: Maybe String)

-- |
-- Representation for a data item
data RecordData a =
  Record [a]
  deriving (Eq, Show)

type Record = RecordData Field

instance ToJSON (RecordData Field) where
  toJSON (Record r) = Object (aesonify r)

instance FromJSON (RecordData Field) where
  parseJSON (Object obj) = return $ Record (bsonify obj)
  parseJSON _ = fail "empty"

instance Functor RecordData where
  fmap f (Record xs) = Record (f <$> xs)

-- |
-- Representation for an API error
data ApiError = ApiError
  { _message :: String
  } deriving (Eq, Show, Generic)

instance ToJSON ApiError

-- |
-- The result of a record validation
data ValidationResult
  = RecordValid
  | ValidationErrors [(Text, Text)]

-- |
-- Representation for an API item result
-- An item result could be an error or a record
data ApiItem a
  = Fail ApiError
  | Succ a
  deriving (Eq, Show)

instance (ToJSON a) =>
         ToJSON (ApiItem a) where
  toJSON (Fail e) = object ["error" .= toJSON e]
  toJSON (Succ a) = toJSON a

-- |
-- Validate a data item against it's definition
validate :: RecordDefinition -> Record -> ValidationResult
validate def r = toResult (validateField def r <$> names)
  where
    names = Map.keys def ++ recordLabels r
    toResult [] = RecordValid
    toResult xs = ValidationErrors xs

validateField :: RecordDefinition -> Record -> Label -> (Text, Text)
validateField def r name
  | not (Map.member name def) = (name, "Field is not allowed")
  | isRequired && not exists = (name, "Field is required")
  where
    maybeField = getField r name
    isRequired = fieldRequired $ fromJust (Map.lookup name def)
    exists = isJust maybeField

recordLabels :: Record -> [Label]
recordLabels (Record xs) = label <$> xs

-- |
-- Lens for record objects
-- Non-existent fields will yield @Null@ on @view@ and will
-- cause a field to be added on @over@
recLens
  :: Functor f
  => Label -> (Field -> f Field) -> Record -> f Record
recLens l f r =
  (\nf -> setField r <$> nf) (f $ fromMaybe (l =: BSON.Null) (getField r l))

-- |
-- Lens used for deleting fields from a record
delLens
  :: Functor f
  => Label -> (Field -> f Field) -> Record -> f Record
delLens l f r = (\nf -> const (delField r l) <$> nf) (f (l =: BSON.Null))

-- |
-- Get the value of a field in a record
-- For non-existent fields a value of @Null@ will be returned
(^=.)
  :: Val a
  => Record -> Label -> Maybe a
(^=.) r l = BSON.cast (value $ view (recLens l) r)

-- |
-- Set the value of a field in a record
-- Existing fields are overwritten and non-existent ones are added
(.=~)
  :: Val v
  => Label -> v -> Record -> Record
(.=~) l v = over (recLens l) (const (l =: v))

-- |
-- Delete the value of a field in a record
(./~) :: Record -> Label -> Record
(./~) r l = over (delLens l) (const (l =: BSON.Null)) r

-- |
-- Set a record field
setField :: Record -> Field -> Record
setField r x = modField (label x) r (const $ Just x) (Just x)

-- |
-- Delete a record field
delField :: Record -> Label -> Record
delField r l = modField l r (const Nothing) Nothing

-- |
-- Modify a record field
modField :: Label -> Record -> (Field -> Maybe Field) -> Maybe Field -> Record
modField l (Record xs) f empty = Record (mod xs)
  where
    mod [] = maybeToList empty
    mod (a:as)
      | label a == l = maybeToList (f a) ++ as
      | otherwise = a : mod as

-- |
-- Get a record field
getField :: Record -> Label -> Maybe Field
getField (Record xs) l = find ((== l) . label) xs

-- |
-- Get the value of a record field
getValue
  :: Val a
  => Text -> Record -> Maybe a
getValue name r = r ^=. name

-- |
-- Set the value of a record field
setValue
  :: Val a
  => Text -> Record -> a -> Record
setValue name r v = r & name .=~ v

-- |
-- Set the value of the updatedAt field
setUpdatedAt :: Record -> IO Record
setUpdatedAt r = do
  time <- getCurrentTime
  return (setValue "updatedAt" r time)

-- |
-- Set the value of the createdAt field
setCreatedAt :: Record -> IO Record
setCreatedAt r@(Record doc) = do
  currentTime <- getCurrentTime
  let oid = doc !? "_id" :: Maybe ObjectId
  let time = maybe currentTime timestamp oid
  return (setValue "createdAt" r time)
