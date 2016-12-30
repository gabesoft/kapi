{-# LANGUAGE OverloadedStrings #-}

-- |
-- Tests for Persistence.Common
module Main
  ( main
  ) where

import Data.Bson
import qualified Data.Map.Strict as Map
import Mocks.Common
import Persistence.Common
import Test.Hspec
import Types.Common

main :: IO ()
main = hspec $ describe "Types.Common" $ do
  -- merge
  it "can merge simple records" $ verifyMerge rec1 rec2 mergeRes1
  it "can merge nested records 1" $ verifyMerge rec3 rec4 mergeRes2
  it "can merge nested records 2" $ verifyMerge rec4 rec5 rec5
  -- exclude
  it "can exclude simple fields" $ verifyExclude rec1 excludeRes1 ["email", "y"]
  it "can exclude nested fields" $ verifyExclude rec3 excludeRes2 ["email", "nested.guid", "nested.none"]
  -- get
  it "can get simple fields - existing" $ verifyGetField rec1 "y" (Just $ mkStrField "y" "2")
  it "can get simple fields - missing" $ verifyGetField rec1 "z" Nothing
  it "can get nested fields - existing" $ verifyGetField rec3 "nested.name" (Just $ mkStrField "name" "Alfred")
  it "can get nested fields - missing" $ verifyGetField rec3 "nested.age" Nothing
  it "can get simple values - existing" $ verifyGetValue rec1 "y" (Just "2" :: Maybe String)
  it "can get simple values - missing" $ verifyGetValue rec1 "z" (Nothing :: Maybe String)
  it "can get nested values - existing" $ verifyGetValue rec3 "nested.name" (Just "Alfred" :: Maybe String)
  it "can get nested values - missing" $ verifyGetValue rec3 "nested.age" (Nothing :: Maybe String)
  -- set
  it "can set simple fields - overwrite" $ verifySetField rec1 (mkStrField "_id" "123") setRes1
  it "can set simple fields - create" $ verifySetField rec1 (mkStrField "x" "3") setRes2
  it "can set simple values - overwrite" $ verifySetValue rec1 "_id" ("123" :: String) setRes1
  it "can set simple values - create" $ verifySetValue rec1 "x" ("3" :: String) setRes2
  it "can set nested values - create" $ verifySetValue rec5 "nested.a" ("1" :: String) setRes3
  it "can set nested values - create" $ verifySetValue rec1 "nested.a.b.c" ("1" :: String) setRes4
  it "can set nested values - create" $ verifySetValue rec4 "nested.a" ("1" :: String) setRes5
  it "can set nested values - overwrite" $ verifySetValue rec4 "nested.guid" ("1" :: String) setRes6
  -- delete
  it "can delete simple fields - existing" $ verifyDelField rec1 "email" delRes1
  it "can delete simple fields - missing" $ verifyDelField rec1 "w" rec1
  it "can delete nested fields - existing" $ verifyDelField rec4 "nested.guid" delRes2
  it "can delete nested fields - missing" $ verifyDelField rec4 "nested.w" rec4
  it "can delete simple values - existing" $ verifyDelValue rec1 "email" delRes3
  it "can delete simple values - missing" $ verifyDelValue rec1 "w" delRes4
  it "can delete nested values - existing" $ verifyDelValue rec4 "nested.guid" delRes5
  it "can delete nested values - missing" $ verifyDelValue rec4 "nested.w" delRes6
  -- has
  it "can determine if a simple field exists - 1 int" $ verifyHasField rec6 "a" True
  it "can determine if a simple field exists - 1 rec" $ verifyHasField rec6 "d" True
  it "can determine if a simple field exists - 1 null" $ verifyHasField rec6 "b" True
  it "can determine if a simple field exists - 0" $ verifyHasField rec6 "x" False
  it "can determine if a nested field exists - 1" $ verifyHasField rec6 "d.f.g" True
  it "can determine if a nested field exists - 0" $ verifyHasField rec6 "d.f.x" False
  it "can determine if a simple value exists - 1 int" $ verifyHasValue rec6 "a" True
  it "can determine if a simple value exists - 1 rec" $ verifyHasValue rec6 "d" True
  it "can determine if a simple value exists - 1 null" $ verifyHasValue rec6 "b" False
  it "can determine if a simple value exists - 0" $ verifyHasValue rec6 "x" False
  it "can determine if a nested value exists - 1" $ verifyHasValue rec6 "d.f.g" True
  it "can determine if a nested value exists - 0" $ verifyHasValue rec6 "d.f.x" False
  it "can determine if a nested value exists - 0" $ verifyHasValue rec6 "d.f.h" False
  it "can populate defaults" $ verifyPopulateDefaults def1 rec7 popRes1
  it "validates a record against a definition" $ verifyValidate def2 rec8 valRes1

verifyMerge :: Record -> Record -> Record -> Expectation
verifyMerge r1 r2 exp = mergeRecords r1 r2 `shouldBe` exp

verifyExclude :: Record -> Record -> [Label] -> Expectation
verifyExclude r exp labels = excludeFields labels r `shouldBe` exp

verifyGetField :: Record -> Label -> Maybe Field -> Expectation
verifyGetField r name exp = getField name r `shouldBe` exp

verifyGetValue :: Val a => Record -> Label -> Maybe a -> Expectation
verifyGetValue r name exp = getValue name r `shouldBe` exp

verifySetField :: Record -> Field -> Record -> Expectation
verifySetField r field exp = setField field r `shouldBe` exp

verifySetValue :: Val a => Record -> Label -> a -> Record -> Expectation
verifySetValue r name value exp = setValue name value r `shouldBe` exp

verifyDelField :: Record -> Label -> Record -> Expectation
verifyDelField r name exp = delField name r `shouldBe` exp

verifyDelValue :: Record -> Label -> Record -> Expectation
verifyDelValue r name exp = delValue name r `shouldBe` exp

verifyHasField :: Record -> Label -> Bool -> Expectation
verifyHasField r name exp = hasField name r `shouldBe` exp

verifyHasValue :: Record -> Label -> Bool -> Expectation
verifyHasValue r name exp = hasValue name r `shouldBe` exp

verifyPopulateDefaults :: RecordDefinition -> Record -> Record -> Expectation
verifyPopulateDefaults def r exp = populateDefaults def r `shouldBe` exp

verifyValidate :: RecordDefinition -> Record -> ValidationResult -> Expectation
verifyValidate def r exp = snd (validate def r) `shouldBe` exp
