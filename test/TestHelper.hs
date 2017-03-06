{-# LANGUAGE OverloadedStrings #-}

-- | Helper functions for tests
module TestHelper where

import Data.Bson
import Data.Maybe
import Data.Text (Text, unpack)
import Data.Time (UTCTime)
import Data.Time.ISO8601
import Persistence.Common
import Test.Hspec
import Types.Common

dateReplacement :: Text
dateReplacement = "2016-09-27T04:39:31.460Z"

emptyRecord :: RecordData a
emptyRecord = Record []

replaceUTCDate :: Label -> Record -> Record
replaceUTCDate name = modField name setTime
  where
    setTime :: Maybe UTCTime -> Maybe UTCTime
    setTime field = const (date dateReplacement) <$> field

replaceTextDate :: Label -> Record -> Record
replaceTextDate name = modField name setText
  where
    setText :: Maybe Text -> Maybe Text
    setText field = const dateReplacement <$> field

shouldMatchRecord :: Record -> Record -> Expectation
shouldMatchRecord r1 r2 = getDocument r1 `shouldMatchList` getDocument r2

mkId :: RecordId -> Value
mkId v = ObjId (read $ unpack v)

mkObjId :: RecordId -> Field
mkObjId = mkObjId' "_id"

date :: Text -> UTCTime
date = fromJust . parseISO8601 . unpack

dateTerm :: Text -> FilterTerm
dateTerm = TermDate . date

mkObjId' :: Label -> RecordId -> Field
mkObjId' name v = name =: mkId v

mkRecId :: RecordId -> Field
mkRecId = mkTxtField "_id"

mkStrField :: Label -> String -> Field
mkStrField name v = name =: v

mkTxtField :: Label -> Text -> Field
mkTxtField name v = name =: v

mkStrListField :: Label -> [String] -> Field
mkStrListField name v = name =: v

mkIntField :: Label -> Int -> Field
mkIntField name v = name =: v

mkFloatField :: Label -> Double -> Field
mkFloatField name v = name =: v

mkBoolField :: Label -> Bool -> Field
mkBoolField name v = name =: v

fromRight :: (Show a, Show b) => Either a b -> b
fromRight (Right x) = x
fromRight y = error $ "Right value expected. " ++ show y
