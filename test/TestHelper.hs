{-# LANGUAGE OverloadedStrings #-}

-- | Helper functions for tests
module TestHelper where

import Data.Bson
import Data.Maybe
import Data.String
import Data.Text (Text, unpack, pack)
import Data.Time (UTCTime)
import Data.Time.ISO8601
import Persistence.Common
import Types.Common

replaceDate :: Label -> Record -> Record
replaceDate name = modField name set
  where
    set :: Maybe UTCTime -> Maybe Text
    set field = const "12345" <$> field

mkId :: RecordId -> Value
mkId v = ObjId (read $ unpack v)

mkObjId :: RecordId -> Field
mkObjId = mkObjId' "_id"

date = fromJust . parseISO8601 . unpack

dateTerm = TermDate . date

mkObjId' :: Label -> RecordId -> Field
mkObjId' name v = name =: mkId v

mkRecId :: RecordId -> Field
mkRecId = mkTxtField "_id"

mkStrField :: Label -> String -> Field
mkStrField name val = name =: val

mkTxtField :: Label -> Text -> Field
mkTxtField name val = name =: val

mkStrListField :: Label -> [String] -> Field
mkStrListField name val = name =: val

mkIntField :: Label -> Int -> Field
mkIntField name val = name =: val

mkFloatField :: Label -> Double -> Field
mkFloatField name val = name =: val

mkBoolField :: Label -> Bool -> Field
mkBoolField name val = name =: val

fromRight :: (Show a, Show b) => Either a b -> b
fromRight (Right x) = x
fromRight y = error $ "Right value expected. " ++ show y
