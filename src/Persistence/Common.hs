{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Common functionality for the persistence layer
module Persistence.Common where

import Control.Monad (join, (>=>))
import Control.Monad.IO.Class
import Data.Aeson as AESON
import Data.Bson as BSON
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Digest.Pure.SHA
import Data.List (find, findIndex, foldl, inits, nub, (\\))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.MongoDB (Database)
import Network.HTTP.Types.Status
import Types.Common

createdAtLabel :: Label
createdAtLabel = "createdAt"

updatedAtLabel :: Label
updatedAtLabel = "updatedAt"

idLabel :: Label
idLabel = "_id"

-- ^
-- Extract the record fields
getDocument :: Record -> Document
getDocument (Record xs) = xs

-- ^
-- Case analysis for the @ApiItem@ type
apiItem :: (e -> c) -> (a -> c) -> ApiItem e a -> c
apiItem f _ (Fail e) = f e
apiItem _ g (Succ a) = g a

-- ^
-- Create a field definition
mkFieldDef
  :: Val a
  => Label -> Bool -> Bool -> Maybe a -> (Label, FieldDefinition)
mkFieldDef name required isId defaultValue =
  (name, FieldDefinition name required (val <$> defaultValue) isId)

-- ^
-- Create a definition for an id field used as a foreign key
mkIdDef :: Label -> (Label, FieldDefinition)
mkIdDef name = mkFieldDef name True True (Nothing :: Maybe String)

-- ^
-- Create a definition for a required field without a default value
mkReqDef' :: Label -> (Label, FieldDefinition)
mkReqDef' name = mkReqDef name (Nothing :: Maybe String)

-- ^
-- Create a definition for a required field
mkReqDef
  :: Val a
  => Label -> Maybe a -> (Label, FieldDefinition)
mkReqDef name = mkFieldDef name True False

-- ^
-- Create a definition for an optional field without a default value
mkOptDef' :: Label -> (Label, FieldDefinition)
mkOptDef' name = mkOptDef name (Nothing :: Maybe String)

-- ^
-- Create a definition for an optional field
mkOptDef
  :: Val a
  => Label -> Maybe a -> (Label, FieldDefinition)
mkOptDef name = mkFieldDef name False False

-- ^
-- Get the MongoDB database name for an app name
-- from a configuration object
confGetDb :: AppName -> ApiConfig -> Database
confGetDb name = fromJust . Map.lookup name . mongoDbs

-- ^
-- Get the elastic-search index name for an app name
-- from a configuration object
confGetEsIndex :: AppName -> ApiConfig -> Text
confGetEsIndex name = fromJust . Map.lookup name . esIndices

-- ^
-- Validate a record against it's definition
validateRecord :: RecordDefinition -> Record -> (Record, ValidationResult)
validateRecord def r = (r, ValidationErrors $ catMaybes results)
  where
    names = nub $ Map.keys (recordFields def) ++ recordLabels r
    results = validateField True def r <$> names

validateField :: Bool -> RecordDefinition -> Record -> Label -> Maybe Field
validateField ignoreId def r name
  | ignoreId && name == idLabel = Nothing
  | name `elem` ignore = Nothing
  | not (Map.member name fields) = Just (mkField "Field is not allowed")
  | isRequired && not (hasValue name r) && noDefault =
    Just (mkField "Field is required")
  | otherwise = Nothing
  where
    fields = recordFields def
    ignore = [updatedAtLabel, createdAtLabel]
    fieldDef = fromJust $ Map.lookup name fields
    isRequired = fieldRequired fieldDef
    noDefault = isNothing (fieldDefault fieldDef)
    mkField :: String -> Field
    mkField msg = name =: msg

-- ^
-- Populate defaults for all missing fields that have a default value
populateDefaults :: RecordDefinition -> Record -> Record
populateDefaults def r = Map.foldl populate r defaults
  where
    defaults = Map.filter (isJust . fieldDefault) (recordFields def)
    populate acc field = modField (fieldLabel field) (set field) acc
    set field = fromMaybe (fieldDefault field)

-- ^
-- Get the names of all fields in a record
recordLabels :: Record -> [Label]
recordLabels (Record xs) = label <$> xs

-- ^
-- Set the value of the id field
setIdValue
  :: Val a
  => a -> Record -> Record
setIdValue = setValue idLabel

-- ^
-- Get the value of the id field
getIdValue :: Record -> Maybe RecordId
getIdValue = getValue idLabel

-- ^
-- Get the value of the id field
getIdValue' :: Record -> RecordId
getIdValue' = fromJust . getValue idLabel

-- ^
-- Return true if the value of a field is a 'True' value
isValueOn :: Label -> Record -> Bool
isValueOn label record = isJust val && fromJust val
  where
    val :: Maybe Bool
    val = getValue label record

-- ^
-- Set the value of a field to the current time
setTimestamp'
  :: MonadIO m
  => Text -> Record -> m Record
setTimestamp' name r = do
  time <- liftIO getCurrentTime
  return (setValue name time r)

-- ^
-- Set the value of the updatedAt field
setUpdatedAt
  :: MonadIO m
  => Record -> m Record
setUpdatedAt = setTimestamp' updatedAtLabel

-- ^
-- Set the value of the createdAt field
setCreatedAt
  :: MonadIO m
  => Record -> m Record
setCreatedAt = setTimestamp' createdAtLabel

-- ^
-- Set the updatedAt field. For new records also set a createdAt field.
setTimestamp
  :: MonadIO m
  => Bool -> Record -> m Record
setTimestamp isNew =
  if isNew
    then setUpdatedAt >=> setCreatedAt
    else setUpdatedAt

-- ^
-- Modify the value of a field or remove it
modField
  :: (Val a, Val b)
  => Label -> (Maybe a -> Maybe b) -> Record -> Record
modField name f r =
  case f (getValue name r) of
    Nothing -> delField name r
    Just v -> setValue name v r

-- ^
-- Change the name of a field if it exists
renameField :: Label -> Label -> Record -> Record
renameField old new (Record doc) = withField old doc Record rename
  where
    rename :: ([Field], Field, [Field]) -> (Int, Document) -> Record
    rename (xs, _ := v, ys) _ = Record $ xs ++ [new := v] ++ ys

-- ^
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
  where
    findField _ [] = error "no field specified"
    findField doc [fname] = find ((fname ==) . label) doc
    findField doc (fname:ns) = withField fname doc (const Nothing) (process ns)
    process ns (_, y, _) _ = findNested y (head ns)
    findNested f@(k := v) fname
      | valIsDoc v = getField fname (docToRec k f)
      | otherwise = Nothing

-- ^
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
    get (_ := v) = cast v

-- ^
-- Get the value of a field by name. Nested fields are supported.
getValue' :: Val a => Label -> Record -> a
getValue' name = fromJust . getValue name

-- ^
-- Set a field in a record. Overwrite the existing value if any.
setField :: Field -> Record -> Record
setField field = flip mergeRecords (Record [field])

-- ^
-- Set the value of a field in a record. Create the field if necessary.
setValue
  :: Val a
  => Label -> a -> Record -> Record
setValue fname fvalue = setField (mkField $ T.split (== '.') fname)
  where
    mkField [] = error "no field name specified"
    mkField [name] = name =: fvalue
    mkField (name:ns) = name =: mkField ns

-- ^
-- Delete a field by name
delField :: Label -> Record -> Record
delField = excludeFields . (: [])

-- ^
-- Set the value of a field to @Null@.
-- If the field does not exist it will be created.
delValue :: Label -> Record -> Record
delValue = flip setValue BSON.Null

-- ^
-- Determine whether a field exists within a record
hasField :: Label -> Record -> Bool
hasField name = isJust . getField name

-- ^
-- Determine whether a field exists within a record
-- and it has a non-null value
hasValue :: Label -> Record -> Bool
hasValue name r = hasField name r && has (getValue name r)
  where
    has Nothing = False
    has (Just BSON.Null) = False
    has _ = True

-- ^
-- Merge two records with the record on the right overwriting any
-- existing fields in the left record. Nested records are supported.
mergeRecords :: Record -> Record -> Record
mergeRecords (Record d1) (Record d2) = Record $ foldl add d1 d2
  where
    add doc field@(k := _) = withField k doc (++ [field]) (process field)
    process field (xs, _, ys) (i, d) = xs ++ [new (d !! i) field] ++ ys
    new lf@(lk := lv) rf@(rk := rv)
      | valIsDoc lv && valIsDoc rv =
        rk := recToDoc (mergeRecords (docToRec lk lf) (docToRec rk rf))
      | otherwise = rk := rv

-- ^
-- Replace one record with another keeping some fields unmodified
replaceRecords :: [Label] -> Record -> Record -> Record
replaceRecords preserveLabels r1 r2 = exclude' (mergeRecords r1 r2')
  where
    r2' = excludeFields preserveLabels r2
    keep name = elem name (getLabels' r2) || elem name preserveLabels
    exclude' r = excludeFields (filter (not . keep) (getLabels' r)) r

-- ^
-- Exclude all specified fields from a record
excludeFields :: [Label] -> Record -> Record
excludeFields labels (Record d) =
  Record $ foldl remove d (splitAtDot <$> labels)
  where
    remove _ [] = error "no field name specified"
    remove doc [name] = filter (\(k := _) -> k /= name) doc
    remove doc (name:ns) = withField name doc id (process ns)
    process ns (xs, y, ys) _ = xs ++ [removeNested y ns] ++ ys
    removeNested f@(k := v) names
      | valIsDoc v = k := recToDoc (excludeFields names (docToRec k f))
      | otherwise = k := v

-- ^
-- Include only the specified fields and the id in a record
includeFields :: [Label] -> Record -> Record
includeFields [] record = record
includeFields labels record = excludeFields (getLabels' record \\ include') record
  where
    sep = "."
    split = filter (not . null) . inits . T.splitOn sep
    addPrefixes xs acc = acc ++ (T.intercalate sep <$> split xs)
    include' = foldr addPrefixes [] labels

-- ^
-- Prepare the labels of the fields to be included in an object
-- which will contain the input fields and the id. Empty input fields
-- will result in all fields being returned.
mkIncludeLabels :: [Label] -> [Label]
mkIncludeLabels [] = []
mkIncludeLabels xs = idLabel : xs

-- ^
-- Extract the field names from a record
getLabels :: Record -> [Label]
getLabels = fmap label . getDocument

-- ^
-- Extract the field names from a record and all of its children
getLabels' :: Record -> [Label]
getLabels' (Record d) = foldl add (label <$> d) d
  where
    pre k l = T.concat [k, ".", l]
    add labels f@(k := v)
      | valIsDoc v = labels ++ (pre k <$> getLabels' (docToRec k f))
      | otherwise = labels

-- ^
-- Find the field with @name@ in @doc@ and do a case analysis
-- based on whether the field is found or not
withField
  :: Text
  -> Document
  -> (Document -> a)
  -> (([Field], Field, [Field]) -> (Int, Document) -> a)
  -> a
withField name doc f g =
  case findIndex ((name ==) . label) doc of
    Nothing -> f doc
    Just i ->
      let (xs, y:ys) = splitAt i doc
      in g (xs, y, ys) (i, doc)

-- ^
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

-- ^
-- Determine whether a bson value is of type @Document@
valIsDoc :: BSON.Value -> Bool
valIsDoc (Doc _) = True
valIsDoc _ = False

-- ^
-- Convert a record to a document value
recToDoc :: Record -> BSON.Value
recToDoc = Doc . getDocument

-- ^
-- Extract the document from a field value and convert it to a record
docToRec :: Label -> Field -> Record
docToRec name field = Record (at name [field])

-- ^
-- Compute the sha-1 hash of a record
recToSha' :: Record -> Digest SHA1State
recToSha' = sha1 . encode

-- ^
-- Compute the sha-1 hash of a record as a string
recToSha :: Record -> String
recToSha = showDigest . recToSha'

-- ^
-- Compute the pagination data given a current page,
-- the page size, and the total number of records
paginate :: Int -> Int -> Int -> Pagination
paginate page' size' total' =
  Pagination
  { paginationTotal = total
  , paginationPage = page
  , paginationSize = size
  , paginationNext = next
  , paginationPrev = prev
  , paginationFirst = first
  , paginationLast = last'
  , paginationStart = start
  , paginationLimit = limit
  }
  where
    total = max total' 0
    size = max size' 1
    first = 1
    last' = max 1 (div total size + min 1 (mod total size))
    page = min (max page' first) last'
    next = min (page + 1) last'
    prev = max first (page - 1)
    start = max 0 ((page - 1) * size)
    limit = size

-- ^
-- Make a sort expression
--
-- >>> mkSortExpr "+title"
-- Just (SortExpr "title" SortAscending)
--
-- >>> mkSortExpr "-title"
-- Just (SortExpr "title" SortDescending)
--
-- >>> mkSortExpr ""
-- Nothing
--
mkSortExpr :: Text -> Maybe SortExpr
mkSortExpr name
  | T.null name = Nothing
  | T.head name == '-' = Just $ SortExpr (getName name) SortDescending
  | otherwise = Just $ SortExpr (getName name) SortAscending
  where
    getName xs
      | T.head xs == '+' || T.head xs == '-' = T.tail xs
      | otherwise = xs

-- ^
-- Create an 'ApiError'
-- TODO: Add the original input field
mkApiError :: Status -> String -> ApiError
mkApiError status msg = ApiError Nothing status (LBS.pack msg)

-- ^
-- Create an 'ApiError' with an HTTP status of 400
mkApiError400 :: String -> ApiError
mkApiError400 = mkApiError status400

-- ^
-- Create an 'ApiError' with an HTTP status of 404
mkApiError404 :: ApiError
mkApiError404 = mkApiError status404 mempty

-- ^
-- Create an 'ApiError' with an HTTP status of 404 and a message
mkApiError404' :: String -> ApiError
mkApiError404' = mkApiError status404

-- ^
-- A record definition that contains only a required id field
idDefinition :: RecordDefinition
idDefinition = RecordDefinition mempty mempty $ Map.fromList [mkReqDef' idLabel]