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
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.MongoDB (Database)
import GHC.Generics
import Network.Socket (HostName, PortNumber)

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

data ApiData a
  = Single a
  | Multiple [a]
  deriving (Eq, Show)

instance ToJSON a => ToJSON (ApiData a) where
  toJSON (Single x) = toJSON x
  toJSON (Multiple xs) = toJSON xs

instance FromJSON a => FromJSON (ApiData a) where
  parseJSON o@(AESON.Object obj) = Single <$> parseJSON o
  parseJSON a@(AESON.Array _) = Multiple <$> parseJSON a
  parseJSON _ = fail "Could not parse ApiData"

-- |
-- Representation for a data item
data RecordData a =
  Record [a]
  deriving (Eq, Show)

type RecordId = Text

type Record = RecordData Field

instance ToJSON (RecordData Field) where
  toJSON (Record r) = Object (aesonify r)

instance FromJSON (RecordData Field) where
  parseJSON (Object obj) = return $ Record (bsonify obj)
  parseJSON _ = fail "Could not parse Record"

instance Functor RecordData where
  fmap f (Record xs) = Record (f <$> xs)

-- |
-- Extract the record data contents
recFields :: RecordData a -> [a]
recFields (Record xs) = xs

-- |
-- Representation for an API error
data ApiError = ApiError
  { message :: String
  } deriving (Eq, Show, Generic)

instance ToJSON ApiError

-- |
-- Wrap an error into an @ApiError@
apiErrorWrap :: Show a => a -> ApiError
apiErrorWrap = ApiError . show

-- |
-- The result of a record validation
data ValidationResult
  = RecordValid Record
  | ValidationErrors [(Text, Text)]

instance Show ValidationResult where
  show (RecordValid r) = mempty
  show (ValidationErrors xs) = unlines $ showErr <$> xs
    where
      showErr (name, err) = show $ T.concat [name, ": ", err]

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

type AppName = Text

-- |
-- Api configuration data
data ApiConfig = ApiConfig
  { apiPort :: PortNumber
  , mongoHost :: HostName
  , mongoPort :: PortNumber
  , mongoDbs :: Map.Map AppName Database
  } deriving (Eq, Show)

-- |
-- Get the MongoDB database name for an app name
-- from a configuration object
confGetDb :: AppName -> ApiConfig -> Database
confGetDb name = fromJust . Map.lookup name . mongoDbs

-- |
-- Validate a data item against it's definition
validate :: RecordDefinition -> Record -> ValidationResult
validate def r = toResult $ catMaybes (validateField def r <$> names)
  where
    names = Map.keys def ++ recordLabels r
    toResult [] = RecordValid r
    toResult xs = ValidationErrors xs

validateField :: RecordDefinition -> Record -> Label -> Maybe (Text, Text)
validateField def r name
  | not (Map.member name def) = Just (name, "Field is not allowed")
  | isRequired && notFound && noDefault = Just (name, "Field is required")
  | otherwise = Nothing
  where
    fieldDef = fromJust $ Map.lookup name def
    maybeField = getField r name
    isRequired = fieldRequired fieldDef
    notFound = isNothing maybeField
    noDefault = isNothing (fieldDefault fieldDef)

-- |
-- Populate defaults for all missing fields that have a default value
populateDefaults :: RecordDefinition -> Record -> Record
populateDefaults def r = Map.foldl populateDef r defaults
  where
    defaults = Map.filter (isJust . fieldDefault) def
    populateDef acc field =
      case getField acc (fieldLabel field) of
        Nothing -> setField acc (fromJust $ fieldDefault field)
        _ -> acc

-- |
-- Get the names of all fields in a record
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
modField l (Record xs) f empty = Record (modify xs)
  where
    modify [] = maybeToList empty
    modify (a:as)
      | label a == l = maybeToList (f a) ++ as
      | otherwise = a : modify as

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

-- |
-- Modify the value of a field or remove it
mapField
  :: (Val a, Val b)
  => Label -> (Maybe a -> Maybe b) -> Record -> Record
mapField l f r =
  case f (getValue l r) of
    Nothing -> delField r l
    Just v -> setValue l r v
