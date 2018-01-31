{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Tests for Persistence.Common
module Main (main) where

import Data.Bson
import Mocks.Common
import Persistence.Common
import Test.Hspec
import Test.QuickCheck
import TestHelper
import Types.Common

main :: IO ()
main =
  hspec $
  describe "Persistence.Common" $ do
    describe "merge" $ do
      it "can merge simple records" $ verifyMerge rec1 rec2 mergeRes1
      it "can merge nested records 1" $ verifyMerge rec3 rec4 mergeRes2
      it "can merge nested records 2" $ verifyMerge rec4 rec5 rec5
      it "can merge nested records 3" $ verifyMerge rec12 rec3 mergeRes3

    describe "replace" $ do
      it "simple records" $
        verifyReplace ["x", "y", "e.r"] rec10 rec11 replaceRes1
      it "simple records keep inner object" $
        verifyReplace ["x", "y", "e"] rec10 rec11 replaceRes2

    describe "exclude" $ do
      it "simple fields" $
        verifyExclude rec1 excludeRes1 ["email", "y"]
      it "nested fields" $
        verifyExclude rec3 excludeRes4 ["email", "nested.guid", "nested.none"]
      it "nested object" $
        verifyExclude rec3 excludeRes5 ["email", "nested"]

    describe "include" $ do
      it "simple fields" $ verifyInclude rec1 excludeRes1 ["_id"]
      it "nested object" $ verifyInclude rec3 excludeRes3 ["nested"]
      it "nested fields" $ verifyInclude rec3 excludeRes2 ["nested._id", "nested.name"]
      it "includes nothing if fields are empty" $ verifyInclude rec3 emptyRecord []
      it "does not include the id if not in the list" $
        verifyInclude rec4 includeRes1 ["nested.guid"]

    describe "get" $ do
      it "can get simple fields - existing" $
        verifyGetField rec1 "y" (Just $ mkStrField "y" "2")
      it "can get simple fields - missing" $ verifyGetField rec1 "z" Nothing
      it "can get nested fields - existing" $
        verifyGetField rec3 "nested.name" (Just $ mkStrField "name" "Alfred")
      it "can get nested fields - missing" $
        verifyGetField rec3 "nested.age" Nothing
      it "can get simple values - existing" $
        verifyGetValue rec1 "y" (Just "2" :: Maybe String)
      it "can get simple values - missing" $
        verifyGetValue rec1 "z" (Nothing :: Maybe String)
      it "can get nested values - existing" $
        verifyGetValue rec3 "nested.name" (Just "Alfred" :: Maybe String)
      it "can get nested values - missing" $
        verifyGetValue rec3 "nested.age" (Nothing :: Maybe String)

    describe "set" $ do
      it "can set simple fields - overwrite" $
        verifySetField rec1 (mkStrField "_id" "123") setRes1
      it "can set simple fields - create" $
        verifySetField rec1 (mkStrField "x" "3") setRes2
      it "can set simple values - overwrite" $
        verifySetValue rec1 "_id" ("123" :: String) setRes1
      it "can set simple values - create" $
        verifySetValue rec1 "x" ("3" :: String) setRes2
      it "can set nested values - create" $
        verifySetValue rec5 "nested.a" ("1" :: String) setRes3
      it "can set nested values - create" $
        verifySetValue rec1 "nested.a.b.c" ("1" :: String) setRes4
      it "can set nested values - create" $
        verifySetValue rec4 "nested.a" ("1" :: String) setRes5
      it "can set nested values - overwrite" $
        verifySetValue rec4 "nested.guid" ("1" :: String) setRes6

    describe "delete" $ do
      it "can delete simple fields - existing" $
        verifyDelField rec1 "email" delRes1
      it "can delete simple fields - missing" $ verifyDelField rec1 "w" rec1
      it "can delete nested fields - existing" $
        verifyDelField rec4 "nested.guid" delRes2
      it "can delete nested fields - missing" $
        verifyDelField rec4 "nested.w" rec4
      it "can delete simple values - existing" $
        verifyDelValue rec1 "email" delRes3
      it "can delete simple values - missing" $ verifyDelValue rec1 "w" delRes4
      it "can delete nested values - existing" $
        verifyDelValue rec4 "nested.guid" delRes5
      it "can delete nested values - missing" $
        verifyDelValue rec4 "nested.w" delRes6

    describe "renameField" $ do
      it "existing field" $
        verifyRenameField
          "a"
          "b"
          (Record [mkStrField "a" "1"])
          (Record [mkStrField "b" "1"])
      it "non existing field" $
        verifyRenameField
          "x"
          "b"
          (Record [mkStrField "a" "1"])
          (Record [mkStrField "a" "1"])

    describe "has" $ do
      it "can determine if a simple field exists - 1 int" $
        verifyHasField rec6 "a" True
      it "can determine if a simple field exists - 1 rec" $
        verifyHasField rec6 "d" True
      it "can determine if a simple field exists - 1 null" $
        verifyHasField rec6 "b" True
      it "can determine if a simple field exists - 0" $
        verifyHasField rec6 "x" False
      it "can determine if a nested field exists - 1" $
        verifyHasField rec6 "d.f.g" True
      it "can determine if a nested field exists - 0" $
        verifyHasField rec6 "d.f.x" False
      it "can determine if a simple value exists - 1 int" $
        verifyHasValue rec6 "a" True
      it "can determine if a simple value exists - 1 rec" $
        verifyHasValue rec6 "d" True
      it "can determine if a simple value exists - 1 null" $
        verifyHasValue rec6 "b" False
      it "can determine if a simple value exists - 0" $
        verifyHasValue rec6 "x" False
      it "can determine if a nested value exists - 1" $
        verifyHasValue rec6 "d.f.g" True
      it "can determine if a nested value exists - 0" $
        verifyHasValue rec6 "d.f.x" False
      it "can determine if a nested value exists - 0" $
        verifyHasValue rec6 "d.f.h" False

    describe "populateDefaults" $
      it "can populate defaults" $ verifyPopulateDefaults def1 rec7 popRes1

    describe "isValueOn" $ do
      it "isValueOn returns true if a value is on" $
        verifyIsValueOn (Record ["a" =: True]) "a" True
      it "isValueOn returns false if a value is not on" $
        verifyIsValueOn (Record ["a" =: False]) "a" False
      it "isValueOn returns false if a value does not exist" $
        verifyIsValueOn (Record []) "a" False
      it "isValueOn returns false if a value exists but is not a Boolean" $
        verifyIsValueOn (Record [mkStrField "a" "True"]) "a" False

    describe "validate" $ do
      it "validates a record against a definition" $
        verifyValidate def2 rec8 valRes1
      it "validation ignores timestamp fields" $
        verifyValidate def2 rec9 valSuccess

    describe "pagination" $ do
      it "computes pagination - quick-check" $ property prop_pagination
      it "computes pagination" $ verifyPagination 4 3 15 pgeRes1

verifyMerge :: Record -> Record -> Record -> Expectation
verifyMerge r1 r2 expected = mergeRecords r1 r2 `shouldMatchRecord` expected

verifyReplace :: [Label] -> Record -> Record -> Record -> Expectation
verifyReplace preserve r1 r2 expected = replaceRecords preserve r1 r2 `shouldMatchRecord` expected

verifyExclude :: Record -> Record -> [Label] -> Expectation
verifyExclude r expected recLabels = excludeFields recLabels r `shouldMatchRecord` expected

verifyInclude :: Record -> Record -> [Label] -> Expectation
verifyInclude r expected recLabels = includeFields recLabels r `shouldMatchRecord` expected

verifyGetField :: Record -> Label -> Maybe Field -> Expectation
verifyGetField r name expected = getField name r `shouldBe` expected

verifyGetValue
  :: Val a
  => Record -> Label -> Maybe a -> Expectation
verifyGetValue r name expected = getValue name r `shouldBe` expected

verifySetField :: Record -> Field -> Record -> Expectation
verifySetField r field expected = setField field r `shouldBe` expected

verifySetValue
  :: Val a
  => Record -> Label -> a -> Record -> Expectation
verifySetValue r name recVal expected = setValue name recVal r `shouldBe` expected

verifyDelField :: Record -> Label -> Record -> Expectation
verifyDelField r name expected = delField name r `shouldBe` expected

verifyDelValue :: Record -> Label -> Record -> Expectation
verifyDelValue r name expected = delValue name r `shouldBe` expected

verifyHasField :: Record -> Label -> Bool -> Expectation
verifyHasField r name expected = hasField name r `shouldBe` expected

verifyHasValue :: Record -> Label -> Bool -> Expectation
verifyHasValue r name expected = hasValue name r `shouldBe` expected

verifyIsValueOn :: Record -> Label -> Bool -> Expectation
verifyIsValueOn r name expected = isValueOn name r `shouldBe` expected

verifyRenameField :: Label -> Label -> Record -> Record -> Expectation
verifyRenameField old new r expected = renameField old new r `shouldBe` expected

verifyPopulateDefaults :: RecordDefinition -> Record -> Record -> Expectation
verifyPopulateDefaults def r expected = populateDefaults def r `shouldBe` expected

verifyValidate :: RecordDefinition -> Record -> ValidationResult -> Expectation
verifyValidate def r expected = snd (validateRecord def r) `shouldBe` expected

verifyPagination :: Int -> Int -> Int -> Pagination -> Expectation
verifyPagination page size tot expected = paginate page size tot `shouldBe` expected

prop_pagination :: Int -> Int -> Int -> Bool
prop_pagination page' size' total' =
  and
    [ page == min pLast (max page' pFirst)
    , tot == max total' 0
    , size == max size' 1
    , next >= page
    , next <= pLast
    , next - page <= 1
    , prev <= page
    , prev >= pFirst
    , page - prev <= 1
    , pFirst == 1
    , pLast >= 1
    , pLast <= div tot size + 1
    , start == (page - 1) * size
    , limit == size
    ]
  where
    pagination = paginate page' size' total'
    tot = paginationTotal pagination
    page = paginationPage pagination
    size = paginationSize pagination
    next = paginationNext pagination
    prev = paginationPrev pagination
    pFirst = paginationFirst pagination
    pLast = paginationLast pagination
    start = paginationStart pagination
    limit = paginationLimit pagination
