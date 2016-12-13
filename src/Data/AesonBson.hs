{-# LANGUAGE OverloadedStrings #-}

-- |
-- AesonBson
-- original location https://github.com/nh2/aesonbson/blob/master/Data/AesonBson.hs
--
-- | Convert JSON to BSON and the other way around.
--
-- Note that BSON has more data types than JSON,
-- so some BSON to JSON conversions are not bijective and somewhat arbitrary.
--
-- This means that for some BSON objects:
--
-- >bsonify . aesonify /= id
-- >bsonifyValue . aesonifyValue /= id
--
-- We tried to choose sensible translations on those cases.
module Data.AesonBson
  ( aesonify
  , aesonifyValue
  , bsonify
  , bsonifyValue
  ) where

import Data.Aeson.Types as AESON
import Data.Bson as BSON
import qualified Data.HashMap.Strict as HashMap (fromList, toList)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Scientific
import Data.Text.Encoding
import qualified Data.Vector as Vector (fromList, toList)

-- | Converts a JSON value to BSON.
bsonifyValue :: AESON.Value -> BSON.Value
bsonifyValue (Object obj) = Doc $ bsonify obj
bsonifyValue (AESON.Array array) =
  BSON.Array . map bsonifyValue . Vector.toList $ array
bsonifyValue (AESON.String str) = BSON.String str
bsonifyValue (Number n)
  | isInteger n = Int64 (fromMaybe 0 $ toBoundedInteger n)
  | otherwise = Float (toRealFloat n)
bsonifyValue (AESON.Bool b) = BSON.Bool b
bsonifyValue AESON.Null = BSON.Null

-- | Converts a BSON value to JSON.
aesonifyValue :: BSON.Value -> AESON.Value
aesonifyValue (Float f) = toJSON f
aesonifyValue (BSON.String s) = toJSON s
aesonifyValue (Doc doc) = Object $ aesonify doc
aesonifyValue (BSON.Array list) =
  AESON.Array . Vector.fromList $ map aesonifyValue list
aesonifyValue (Bin (Binary binary)) = toJSON (decodeUtf8 binary)
aesonifyValue (Fun (Function function)) = toJSON (decodeUtf8 function)
aesonifyValue (Uuid (UUID uuid)) = toJSON (decodeUtf8 uuid)
aesonifyValue (Md5 (MD5 md5)) = toJSON (decodeUtf8 md5)
aesonifyValue (UserDef (UserDefined userdef)) = toJSON (decodeUtf8 userdef)
aesonifyValue (ObjId oid) = toJSON $ show oid -- Relies on bson to show the OID as 24 digit hex.
                                              -- It would be better if BSON exposed a non-show function for this,
                                              -- preferably a fast one.
aesonifyValue (BSON.Bool bool) = toJSON bool
aesonifyValue (UTC utc) = toJSON utc
aesonifyValue BSON.Null = AESON.Null
aesonifyValue (RegEx (Regex pat mods)) = toJSON $ mconcat ["/", pat, "/", mods]
aesonifyValue (JavaScr (Javascript env code)) =
  object ["environment" .= aesonify env, "code" .= code]
aesonifyValue (Sym (Symbol sym)) = toJSON sym
aesonifyValue (Int32 int32) = toJSON int32
aesonifyValue (Int64 int64) = toJSON int64
aesonifyValue (Stamp (MongoStamp int64)) = toJSON int64
aesonifyValue (MinMax mm) =
  case mm of
    MinKey -> toJSON (-1 :: Int)
    MaxKey -> toJSON (1 :: Int)

-- | Converts an AESON object to a BSON document.
bsonify :: AESON.Object -> BSON.Document
bsonify = map (\(t, v) -> t := bsonifyValue v) . HashMap.toList

-- | Converts a BSON document to an AESON object.
aesonify :: BSON.Document -> AESON.Object
aesonify = HashMap.fromList . map (\(l := v) -> (l, aesonifyValue v))
