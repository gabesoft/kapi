{-# LANGUAGE OverloadedStrings #-}

-- | Helper functions for tests
module TestHelper where

import Data.Bson
import Data.Time (UTCTime)
import Persistence.Common
import Types.Common

modDate :: Label -> Record -> Record
modDate name = modField name set
  where
    set :: Maybe UTCTime -> Maybe String
    set field = const "12345" <$> field

mkId :: String -> Value
mkId v = ObjId (read v)

mkObjId :: String -> Field
mkObjId v = "_id" =: mkId v

mkRecId :: String -> Field
mkRecId = mkStrField "_id"

mkStrField :: Label -> String -> Field
mkStrField name val = name =: val

mkIntField :: Label -> Int -> Field
mkIntField name val = name =: val

mkFloatField :: Label -> Double -> Field
mkFloatField name val = name =: val

mkBoolField :: Label -> Bool -> Field
mkBoolField name val = name =: val
