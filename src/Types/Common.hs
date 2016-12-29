{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Common types
module Types.Common where

import Control.Monad (join)
import Data.Aeson as AESON
import Data.AesonBson (aesonify, bsonify)
import Data.Bifunctor
import Data.Bson as BSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Function ((&))
import Data.List (find, findIndex, foldl)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.MongoDB (Database)
import GHC.Generics
import Network.HTTP.Types.Status
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

instance ToJSON a =>
         ToJSON (ApiData a) where
  toJSON (Single x) = toJSON x
  toJSON (Multiple xs) = toJSON xs

instance FromJSON a =>
         FromJSON (ApiData a) where
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

instance Monoid (RecordData Field) where
  mempty = Record mempty
  mappend (Record xs) (Record ys) = Record (BSON.merge ys xs)

-- |
-- Extract the record data contents
recFields :: RecordData a -> [a]
recFields (Record xs) = xs

-- |
-- Representation for an API error
data ApiError = ApiError
  { apiErrorMessage :: LBS.ByteString
  , apiErrorStatus :: Status
  } deriving (Eq, Show, Generic)

instance ToJSON ApiError where
  toJSON err = object ["message" .= LBS.unpack (apiErrorMessage err)]

-- |
-- The result of a record validation
data ValidationResult =
  ValidationErrors [Field]
  deriving (Eq, Show)

instance ToJSON ValidationResult where
  toJSON (ValidationErrors []) = object []
  toJSON (ValidationErrors xs) = toJSON (Record xs)

instance Monoid ValidationResult where
  mempty = ValidationErrors mempty
  mappend (ValidationErrors xs) (ValidationErrors ys) =
    ValidationErrors (xs <> ys)

-- |
-- Convert the result of a validation to an @Either@ value
vResultToApiItem :: (a, ValidationResult) -> ApiItem ApiError a
vResultToApiItem (a, ValidationErrors []) = Succ a
vResultToApiItem (_, err) = Fail (ApiError (encode err) status400)

-- |
-- Representation for an API item result
-- An item result could be an error or a record
data ApiItem e a
  = Fail e
  | Succ a
  deriving (Eq, Show, Ord)

type ApiResult = ApiItem ApiError Record

instance Bifunctor ApiItem where
  bimap f _ (Fail e) = Fail (f e)
  bimap _ g (Succ a) = Succ (g a)

instance Functor (ApiItem e) where
  fmap _ (Fail e) = Fail e
  fmap f (Succ a) = Succ (f a)

instance Applicative (ApiItem e) where
  pure = Succ
  Fail e <*> _ = Fail e
  Succ f <*> a = fmap f a

instance Monad (ApiItem e) where
  Fail e >>= _ = Fail e
  Succ a >>= k = k a

instance (ToJSON a) =>
         ToJSON (ApiItem ApiError a) where
  toJSON (Fail e) = object ["error" .= toJSON e]
  toJSON (Succ a) = toJSON a

-- |
-- Case analysis for the @ApiItem@ type
apiItem :: (e -> c) -> (a -> c) -> ApiItem e a -> c
apiItem f _ (Fail e) = f e
apiItem _ g (Succ a) = g a

-- |
-- Application name
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
-- Validate a record against it's definition
validate :: RecordDefinition -> Record -> (Record, ValidationResult)
validate def r = (r, ValidationErrors $ catMaybes results)
  where
    names = Map.keys def ++ recordLabels r
    results = validateField True def r <$> names

validateField :: Bool -> RecordDefinition -> Record -> Label -> Maybe Field
validateField ignoreId def r name
  | ignoreId && name == "_id" = Nothing
  | not (Map.member name def) = Just (mkField "Field is not allowed")
  | isRequired && hasValue name r && noDefault =
    Just (mkField "Field is required")
  | otherwise = Nothing
  where
    fieldDef = fromJust $ Map.lookup name def
    isRequired = fieldRequired fieldDef
    noDefault = isNothing (fieldDefault fieldDef)
    mkField :: String -> Field
    mkField msg = name =: msg

-- |
-- Populate defaults for all missing fields that have a default value
populateDefaults :: RecordDefinition -> Record -> Record
populateDefaults def r = Map.foldl populate r defaults
  where
    defaults = Map.filter (isJust . fieldDefault) def
    populate acc field = modField (fieldLabel field) (set field) acc
    set field = fromMaybe (fieldDefault field)

-- |
-- Get the names of all fields in a record
recordLabels :: Record -> [Label]
recordLabels (Record xs) = label <$> xs

-- |
-- Set the value of the id field
setIdValue
  :: Val a
  => a -> Record -> Record
setIdValue = setValue "_id"

-- |
-- Get the value of the id field
getIdValue :: Record -> Maybe RecordId
getIdValue = getValue "_id"

-- |
-- Set the value of the updatedAt field
setUpdatedAt :: Record -> IO Record
setUpdatedAt r = do
  time <- getCurrentTime
  return (setValue "updatedAt" time r)

-- |
-- Set the value of the createdAt field
setCreatedAt :: Record -> IO Record
setCreatedAt r = do
  currentTime <- getCurrentTime
  let oid = getValue "_id" r :: Maybe ObjectId
  let time = maybe currentTime timestamp oid
  return (setValue "createdAt" time r)

-- |
-- Modify the value of a field or remove it
modField
  :: (Val a, Val b)
  => Label -> (Maybe a -> Maybe b) -> Record -> Record
modField name mod r =
  case mod (getValue name r) of
    Nothing -> delField name r
    Just val -> setValue name val r

-- |
-- Get a field by name. Nested fields are supported.
--
-- >>> getField "inner.email" (Record [ inner : [ email : "bson@email.com" ]])
-- Just ("email" : "bson@email.com")
--
-- >>> getField "_id" (Record [_id : "123"])
-- Just (_id : "123")
--
-- >>> getField "email" (Record [_id : "123"])
-- Nothing
--
getField :: Label -> Record -> Maybe Field
getField name (Record d) = findField d (splitAtDot name)
  where findField doc (name:[]) = find ((name ==) . label) doc
        findField doc (name:ns) =
          case findIndex ((name ==) . label) doc of
            Nothing -> Nothing
            Just i ->
              let (_, f:_) = splitAt i doc
              in findNested f (head ns)
        findNested f@(k := v) name
          | valIsDoc v = getField name (docToRec k f)
          | otherwise = Nothing

-- |
-- Get the value of a field by name. Nested fields are supported.
--
-- >>> getValue "inner.email" (Record [ inner : [ email : "bson@email.com" ]])
-- Just "bson@email.com"
--
-- >>> getValue "_id" (Record [_id : "123"])
-- Just "123"
--
-- >>> getValue "email" (Record [_id : "123"])
-- Nothing
--
-- >>> getValue "email" (Record [email : Null])
-- Nothing
--
getValue
  :: Val a
  => Label -> Record -> Maybe a
getValue name r = join $ get <$> getField name r
  where
    get
      :: Val a
      => Field -> Maybe a
    get (k := v) = cast v

-- |
-- Set a field in a record. Overwrite the existing value if any.
setField :: Field -> Record -> Record
setField field = flip mergeRecords (Record [field])

-- |
-- Set the value of a field in a record. Create the field if necessary.
setValue
  :: Val a
  => Label -> a -> Record -> Record
setValue name value = setField (mkField $ T.split (== '.') name)
  where mkField (name:[]) = name =: value
        mkField (name:ns) = name =: mkField ns

-- |
-- Delete a field by name
delField :: Label -> Record -> Record
delField = excludeFields . (:[])

-- |
-- Set the value of a field to @Null@.
-- If the field does not exist it will be created.
delValue :: Label -> Record -> Record
delValue = flip setValue BSON.Null

-- |
-- Determine whether a field exists within a record
hasField :: Label -> Record -> Bool
hasField name = isJust . getField name

-- |
-- Determine whether a field exists within a record
-- and it has a non-null value
hasValue :: Label -> Record -> Bool
hasValue name r = hasField name r && has (getValue name r)
  where
    has Nothing = False
    has (Just BSON.Null) = False
    has _ = True

-- |
-- Merge two records with the record on the right overwriting any
-- existing fields in the left record. Nested records are supported.
mergeRecords :: Record -> Record -> Record
mergeRecords (Record r1) (Record r2) = Record $ foldl add r1 r2
  where
    add doc field@(k := v) =
      case findIndex ((k ==) . label) doc of
        Nothing -> doc ++ [field]
        Just i ->
          let (x, _:y) = splitAt i doc
          in x ++ [new (doc !! i) field] ++ y
    new lf@(lk := lv) rf@(rk := rv)
      | valIsDoc lv && valIsDoc rv =
        rk := recToDoc (mergeRecords (docToRec lk lf) (docToRec rk rf))
      | otherwise = rk := rv

-- |
-- Exclude all specified labels from a record
excludeFields :: [Label] -> Record -> Record
excludeFields labels (Record d) = Record $ foldl remove d (splitAtDot <$> labels)
  where
    remove doc (name:[]) = filter (\(k := _) -> k /= name) doc
    remove doc (name:ns) =
      case findIndex ((name ==) . label) doc of
        Nothing -> doc
        Just i ->
          let (x, y:ys) = splitAt i doc
          in x ++ [removeNested y ns] ++ ys
    removeNested f@(k := v) names
      | valIsDoc v = k := recToDoc (excludeFields names (docToRec k f))
      | otherwise = k := v

-- |
-- Split a nested label at the first dot
--
-- >>> splitAtDot "a.b.c"
-- ["a", "b.c"]
--
-- >>> splitAtDot "a"
-- ["a"]
--
splitAtDot :: Text -> [Text]
splitAtDot name =
  case T.findIndex ('.' ==) name of
    Nothing -> [name]
    Just i ->
      let (x, y) = T.splitAt i name
      in [x, T.tail y]

-- |
-- Determine whether a bson value is of type @Document@
valIsDoc :: BSON.Value -> Bool
valIsDoc (Doc _) = True
valIsDoc _ = False

-- |
-- Convert a record to a document value
recToDoc :: Record -> BSON.Value
recToDoc (Record doc) = Doc doc

-- |
-- Extract the document from a field value and convert it to a record
docToRec :: Label -> Field -> Record
docToRec name field = Record (at name [field])
