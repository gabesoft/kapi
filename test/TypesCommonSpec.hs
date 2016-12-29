{-# LANGUAGE OverloadedStrings #-}

-- |
-- Tests for Types.Common
module Main
  ( main
  ) where

import Data.Bson
import Test.Hspec
import Types.Common

main :: IO ()
main = hspec $ describe "Types.Common" $ do
  -- | merge
  it "can merge simple records" $ verifyMerge rec1 rec2 mergeRes1
  it "can merge nested records 1" $ verifyMerge rec3 rec4 mergeRes2
  it "can merge nested records 2" $ verifyMerge rec4 rec5 rec5
  -- | exclude
  it "can exclude simple fields" $ verifyExclude rec1 excludeRes1 ["email", "y"]
  it "can exclude nested fields" $ verifyExclude rec3 excludeRes2 ["email", "nested.guid", "nested.none"]
  -- | get
  it "can get simple fields - existing" $ verifyGetField rec1 "y" (Just $ mkField "y" "2")
  it "can get simple fields - missing" $ verifyGetField rec1 "z" Nothing
  it "can get nested fields - existing" $ verifyGetField rec3 "nested.name" (Just $ mkField "name" "Alfred")
  it "can get nested fields - missing" $ verifyGetField rec3 "nested.age" Nothing
  it "can get simple values - existing" $ verifyGetValue rec1 "y" (Just "2" :: Maybe String)
  it "can get simple values - missing" $ verifyGetValue rec1 "z" (Nothing :: Maybe String)
  it "can get nested values - existing" $ verifyGetValue rec3 "nested.name" (Just "Alfred" :: Maybe String)
  it "can get nested values - missing" $ verifyGetValue rec3 "nested.age" (Nothing :: Maybe String)
  -- | set
  it "can set simple fields - overwrite" $ verifySetField rec1 (mkField "_id" "123") setRes1
  it "can set simple fields - create" $ verifySetField rec1 (mkField "x" "3") setRes2
  it "can set simple values - overwrite" $ verifySetValue rec1 "_id" ("123" :: String) setRes1
  it "can set simple values - create" $ verifySetValue rec1 "x" ("3" :: String) setRes2
  it "can set nested values - create" $ verifySetValue rec5 "nested.a" ("1" :: String) setRes3
  it "can set nested values - create" $ verifySetValue rec1 "nested.a.b.c" ("1" :: String) setRes4
  it "can set nested values - create" $ verifySetValue rec4 "nested.a" ("1" :: String) setRes5
  it "can set nested values - overwrite" $ verifySetValue rec4 "nested.guid" ("1" :: String) setRes6
  -- | delete
  it "can delete simple fields - existing" $ verifyDelField rec1 "email" delRes1
  it "can delete simple fields - missing" $ verifyDelField rec1 "w" rec1
  it "can delete nested fields - existing" $ verifyDelField rec4 "nested.guid" delRes2
  it "can delete nested fields - missing" $ verifyDelField rec4 "nested.w" rec4
  it "can delete simple values - existing" $ verifyDelValue rec1 "email" delRes3
  it "can delete simple values - missing" $ verifyDelValue rec1 "w" delRes4
  it "can delete nested values - existing" $ verifyDelValue rec4 "nested.guid" delRes5
  it "can delete nested values - missing" $ verifyDelValue rec4 "nested.w" delRes6
  -- | has
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

mkField :: Label -> String -> Field
mkField name val = name =: val

-- TODO move mock data to another file

rec6 :: Record
rec6 =
  Record
    [ "a" =: (1 :: Int)
    , "b" =: Null
    , mkField "c" "3"
    , "d" =: [mkField "e" "4", "f" =: [mkField "g" "5", "h" =: Null]]
    ]

rec1 :: Record
rec1 =
  Record
    [ mkField "_id" "584e58195984185eb8000005"
    , mkField "email" "blue@leaf.com"
    , mkField "y" "2"
    ]

rec2 :: Record
rec2 =
  Record
    [ mkField "_id" "584e58195984185eb8000006"
    , mkField "email" "tree@leaf.com"
    , mkField "x" "1"
    ]

mergeRes1 :: Record
mergeRes1 =
  Record
    [ mkField "_id" "584e58195984185eb8000006"
    , mkField "email" "tree@leaf.com"
    , mkField "y" "2"
    , mkField "x" "1"
    ]

excludeRes1 :: Record
excludeRes1 = Record [mkField "_id" "584e58195984185eb8000005"]

setRes1 :: Record
setRes1 =
  Record
    [ mkField "_id" "123"
    , mkField "email" "blue@leaf.com"
    , mkField "y" "2"
    ]

setRes2 :: Record
setRes2 =
  Record
    [ mkField "_id" "584e58195984185eb8000005"
    , mkField "email" "blue@leaf.com"
    , mkField "y" "2"
    , mkField "x" "3"
    ]

setRes4 :: Record
setRes4 =
  Record
    [ mkField "_id" "584e58195984185eb8000005"
    , mkField "email" "blue@leaf.com"
    , mkField "y" "2"
    , "nested" =: ["a" =: ["b" =: [mkField "c" "1"]]]
    ]

delRes1 :: Record
delRes1 = Record [mkField "_id" "584e58195984185eb8000005", mkField "y" "2"]

delRes3 :: Record
delRes3 =
  Record
    [mkField "_id" "584e58195984185eb8000005", "email" =: Null, mkField "y" "2"]

delRes4 :: Record
delRes4 =
  Record
    [ mkField "_id" "584e58195984185eb8000005"
    , mkField "email" "blue@leaf.com"
    , mkField "y" "2"
    , "w" =: Null
    ]

rec3 :: Record
rec3 =
  Record
    [ mkField "_id" "584e58195984185eb8000007"
    , mkField "email" "bamboo@leaf.com"
    , "nested" =:
      [ mkField "_id" "1234"
      , mkField "guid" "4f24d46e-cd4d-11e6-b6c7-0800276ab015"
      , mkField "name" "Alfred"
      ]
    ]

excludeRes2 :: Record
excludeRes2 =
  Record
    [ mkField "_id" "584e58195984185eb8000007"
    , "nested" =: [mkField "_id" "1234", mkField "name" "Alfred"]
    ]

rec4 :: Record
rec4 =
  Record
    [ mkField "_id" "584e58195984185eb8000008"
    , mkField "email" "bamboo@tree.com"
    , "nested" =:
      [ mkField "_id" "cc151c98-965d-4aed-82c1-a7c71f848250"
      , mkField "guid" "a01c793c-58d2-49d6-aba9-4012e8d6ace5"
      , mkField "alias" "Mambo"
      ]
    ]

rec5 :: Record
rec5 =
  Record
    [ mkField "_id" "584e58195984185eb8000008"
    , mkField "email" "bamboo@tree.com"
    , "nested" =: Null
    ]

mergeRes2 :: Record
mergeRes2 =
  Record
    [ mkField "_id" "584e58195984185eb8000008"
    , mkField "email" "bamboo@tree.com"
    , "nested" =:
      [ mkField "_id" "cc151c98-965d-4aed-82c1-a7c71f848250"
      , mkField "guid" "a01c793c-58d2-49d6-aba9-4012e8d6ace5"
      , mkField "name" "Alfred"
      , mkField "alias" "Mambo"
      ]
    ]

setRes3 :: Record
setRes3 =
  Record
    [ mkField "_id" "584e58195984185eb8000008"
    , mkField "email" "bamboo@tree.com"
    , "nested" =: [mkField "a" "1"]
    ]

setRes5 :: Record
setRes5 =
  Record
    [ mkField "_id" "584e58195984185eb8000008"
    , mkField "email" "bamboo@tree.com"
    , "nested" =:
      [ mkField "_id" "cc151c98-965d-4aed-82c1-a7c71f848250"
      , mkField "guid" "a01c793c-58d2-49d6-aba9-4012e8d6ace5"
      , mkField "alias" "Mambo"
      , mkField "a" "1"
      ]
    ]

setRes6 :: Record
setRes6 =
  Record
    [ mkField "_id" "584e58195984185eb8000008"
    , mkField "email" "bamboo@tree.com"
    , "nested" =:
      [ mkField "_id" "cc151c98-965d-4aed-82c1-a7c71f848250"
      , mkField "guid" "1"
      , mkField "alias" "Mambo"
      ]
    ]

delRes2 :: Record
delRes2 =
  Record
    [ mkField "_id" "584e58195984185eb8000008"
    , mkField "email" "bamboo@tree.com"
    , "nested" =:
      [ mkField "_id" "cc151c98-965d-4aed-82c1-a7c71f848250"
      , mkField "alias" "Mambo"
      ]
    ]

delRes5 :: Record
delRes5 =
  Record
    [ mkField "_id" "584e58195984185eb8000008"
    , mkField "email" "bamboo@tree.com"
    , "nested" =:
      [ mkField "_id" "cc151c98-965d-4aed-82c1-a7c71f848250"
      , "guid" =: Null
      , mkField "alias" "Mambo"
      ]
    ]

delRes6 :: Record
delRes6 =
  Record
    [ mkField "_id" "584e58195984185eb8000008"
    , mkField "email" "bamboo@tree.com"
    , "nested" =:
      [ mkField "_id" "cc151c98-965d-4aed-82c1-a7c71f848250"
      , mkField "guid" "a01c793c-58d2-49d6-aba9-4012e8d6ace5"
      , mkField "alias" "Mambo"
      , "w" =: Null
      ]
    ]
