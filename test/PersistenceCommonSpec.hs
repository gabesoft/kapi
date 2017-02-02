{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Tests for Persistence.Common
module Main (main) where

import Data.Bson
import Data.List (all)
import qualified Data.Map.Strict as Map
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
     -- merge
     it "can merge simple records" $ verifyMerge rec1 rec2 mergeRes1
     it "can merge nested records 1" $ verifyMerge rec3 rec4 mergeRes2
     it "can merge nested records 2" $ verifyMerge rec4 rec5 rec5
     -- replace
     it "can replace simple records" $ verifyReplace ["x","y", "e.r"] rec10 rec11 replaceRes1
     -- exclude
     it "can exclude simple fields" $
       verifyExclude rec1 excludeRes1 ["email", "y"]
     it "can exclude nested fields" $
       verifyExclude rec3 excludeRes4 ["email", "nested.guid", "nested.none"]
     -- include
     it "can include simple fields" $ verifyInclude rec1 excludeRes1 ["_id"]
     it "can include nested fields" $ verifyInclude rec3 excludeRes2 ["nested._id", "nested.name"]
     it "includes all if fields are empty" $ verifyInclude rec3 rec3 []
     it "does not include the id if not in the list" $ verifyInclude rec4 includeRes1 [ "nested.guid" ]
     -- get
     it "can get simple fields - existing" $
       verifyGetField rec1 "y" (Just $ mkStrField "y" "2")
     it "can get simple fields - missing" $ verifyGetField rec1 "z" Nothing
     it "can get nested fields - existing" $
       verifyGetField rec3 "nested.name" (Just $ mkStrField "name" "Alfred")
     it "can get nested fields - missing" $ verifyGetField rec3 "nested.age" Nothing
     it "can get simple values - existing" $
       verifyGetValue rec1 "y" (Just "2" :: Maybe String)
     it "can get simple values - missing" $
       verifyGetValue rec1 "z" (Nothing :: Maybe String)
     it "can get nested values - existing" $
       verifyGetValue rec3 "nested.name" (Just "Alfred" :: Maybe String)
     it "can get nested values - missing" $
       verifyGetValue rec3 "nested.age" (Nothing :: Maybe String)
     -- set
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
     -- delete
     it "can delete simple fields - existing" $ verifyDelField rec1 "email" delRes1
     it "can delete simple fields - missing" $ verifyDelField rec1 "w" rec1
     it "can delete nested fields - existing" $
       verifyDelField rec4 "nested.guid" delRes2
     it "can delete nested fields - missing" $ verifyDelField rec4 "nested.w" rec4
     it "can delete simple values - existing" $ verifyDelValue rec1 "email" delRes3
     it "can delete simple values - missing" $ verifyDelValue rec1 "w" delRes4
     it "can delete nested values - existing" $
       verifyDelValue rec4 "nested.guid" delRes5
     it "can delete nested values - missing" $ verifyDelValue rec4 "nested.w" delRes6
     -- rename field
     it "can rename an existing field" $
       verifyRenameField
         "a"
         "b"
         (Record [mkStrField "a" "1"])
         (Record [mkStrField "b" "1"])
     it "can rename a non existing field" $
       verifyRenameField
         "x"
         "b"
         (Record [mkStrField "a" "1"])
         (Record [mkStrField "a" "1"])
     -- has
     it "can determine if a simple field exists - 1 int" $
       verifyHasField rec6 "a" True
     it "can determine if a simple field exists - 1 rec" $
       verifyHasField rec6 "d" True
     it "can determine if a simple field exists - 1 null" $
       verifyHasField rec6 "b" True
     it "can determine if a simple field exists - 0" $ verifyHasField rec6 "x" False
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
     it "can determine if a simple value exists - 0" $ verifyHasValue rec6 "x" False
     it "can determine if a nested value exists - 1" $
       verifyHasValue rec6 "d.f.g" True
     it "can determine if a nested value exists - 0" $
       verifyHasValue rec6 "d.f.x" False
     it "can determine if a nested value exists - 0" $
       verifyHasValue rec6 "d.f.h" False
     -- populate defaults
     it "can populate defaults" $ verifyPopulateDefaults def1 rec7 popRes1
     -- validate
     it "validates a record against a definition" $ verifyValidate def2 rec8 valRes1
     it "validation ignores timestamp fields" $ verifyValidate def2 rec9 valSuccess
     it "computes pagination - quick-check" $ property prop_pagination
     it "computes pagination" $ verifyPagination 4 3 15 pgeRes1

verifyMerge :: Record -> Record -> Record -> Expectation
verifyMerge r1 r2 exp = mergeRecords r1 r2 `shouldBe` exp

verifyReplace :: [Label] -> Record -> Record -> Record -> Expectation
verifyReplace labels r1 r2 exp = replaceRecords labels r1 r2 `shouldBe` exp

verifyExclude :: Record -> Record -> [Label] -> Expectation
verifyExclude r exp labels = excludeFields labels r `shouldBe` exp

verifyInclude :: Record -> Record -> [Label] -> Expectation
verifyInclude r exp labels = includeFields labels r `shouldBe` exp

verifyGetField :: Record -> Label -> Maybe Field -> Expectation
verifyGetField r name exp = getField name r `shouldBe` exp

verifyGetValue
  :: Val a
  => Record -> Label -> Maybe a -> Expectation
verifyGetValue r name exp = getValue name r `shouldBe` exp

verifySetField :: Record -> Field -> Record -> Expectation
verifySetField r field exp = setField field r `shouldBe` exp

verifySetValue
  :: Val a
  => Record -> Label -> a -> Record -> Expectation
verifySetValue r name value exp = setValue name value r `shouldBe` exp

verifyDelField :: Record -> Label -> Record -> Expectation
verifyDelField r name exp = delField name r `shouldBe` exp

verifyDelValue :: Record -> Label -> Record -> Expectation
verifyDelValue r name exp = delValue name r `shouldBe` exp

verifyHasField :: Record -> Label -> Bool -> Expectation
verifyHasField r name exp = hasField name r `shouldBe` exp

verifyHasValue :: Record -> Label -> Bool -> Expectation
verifyHasValue r name exp = hasValue name r `shouldBe` exp

verifyRenameField :: Label -> Label -> Record -> Record -> Expectation
verifyRenameField old new r exp = renameField old new r `shouldBe` exp

verifyPopulateDefaults :: RecordDefinition -> Record -> Record -> Expectation
verifyPopulateDefaults def r exp = populateDefaults def r `shouldBe` exp

verifyValidate :: RecordDefinition -> Record -> ValidationResult -> Expectation
verifyValidate def r exp = snd (validate def r) `shouldBe` exp

verifyPagination :: Int -> Int -> Int -> Pagination -> Expectation
verifyPagination page size total exp = paginate page size total `shouldBe` exp

prop_pagination :: Int -> Int -> Int -> Bool
prop_pagination page' size' total' =
  and
    [ page == min last (max page' first)
    , total == max total' 0
    , size == max size' 1
    , next >= page
    , next <= last
    , next - page <= 1
    , prev <= page
    , prev >= first
    , page - prev <= 1
    , first == 1
    , last >= 1
    , last <= div total size + 1
    , start == (page - 1) * size
    , limit == size
    ]
  where
    pagination = paginate page' size' total'
    total = paginationTotal pagination
    page = paginationPage pagination
    size = paginationSize pagination
    next = paginationNext pagination
    prev = paginationPrev pagination
    first = paginationFirst pagination
    last = paginationLast pagination
    start = paginationStart pagination
    limit = paginationLimit pagination