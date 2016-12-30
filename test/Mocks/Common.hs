{-# LANGUAGE OverloadedStrings #-}

-- | Common mocks
module Mocks.Common where

import Data.Bson
import qualified Data.Map.Strict as Map
import Types.Common

mkStrField :: Label -> String -> Field
mkStrField name val = name =: val

mkIntField :: Label -> Int -> Field
mkIntField name val = name =: val

mkBoolField :: Label -> Bool -> Field
mkBoolField name val = name =: val

-- | definitions
def1 :: RecordDefinition
def1 =
  Map.fromList
    [ mkOptDef "admin" (Just False)
    , mkReqDef "disabled" (Just False)
    , mkReqDef "count" (Just 1 :: Maybe Int)
    ]

def2 :: RecordDefinition
def2 =
  Map.fromList
    [ mkReqDef "admin" (Just False)
    , mkOptDef "disabled" (Just False)
    , mkReqDef' "count"
    , mkReqDef' "name"
    ]

-- | Records
rec1 :: Record
rec1 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000005"
    , mkStrField "email" "blue@leaf.com"
    , mkStrField "y" "2"
    ]

rec2 :: Record
rec2 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000006"
    , mkStrField "email" "tree@leaf.com"
    , mkStrField "x" "1"
    ]

rec3 :: Record
rec3 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000007"
    , mkStrField "email" "bamboo@leaf.com"
    , "nested" =:
      [ mkStrField "_id" "1234"
      , mkStrField "guid" "4f24d46e-cd4d-11e6-b6c7-0800276ab015"
      , mkStrField "name" "Alfred"
      ]
    ]

rec4 :: Record
rec4 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000008"
    , mkStrField "email" "bamboo@tree.com"
    , "nested" =:
      [ mkStrField "_id" "cc151c98-965d-4aed-82c1-a7c71f848250"
      , mkStrField "guid" "a01c793c-58d2-49d6-aba9-4012e8d6ace5"
      , mkStrField "alias" "Mambo"
      ]
    ]

rec5 :: Record
rec5 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000008"
    , mkStrField "email" "bamboo@tree.com"
    , "nested" =: Null
    ]

rec6 :: Record
rec6 =
  Record
    [ mkIntField "a" 1
    , "b" =: Null
    , mkStrField "c" "3"
    , "d" =: [mkStrField "e" "4", "f" =: [mkStrField "g" "5", "h" =: Null]]
    ]

rec7 :: Record
rec7 = Record [mkStrField "_id" "31889d94-dada-481d-a7d8-0b5e48ee54f3"]

rec8 :: Record
rec8 =
  Record
    [ mkStrField "_id" "31889d94-dada-481d-a7d8-0b5e48ee54f3"
    , "name" =: Null
    , mkStrField "x" "1"
    ]

-- | Delete field resultst

delRes1 :: Record
delRes1 =
  Record [mkStrField "_id" "584e58195984185eb8000005", mkStrField "y" "2"]

delRes2 :: Record
delRes2 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000008"
    , mkStrField "email" "bamboo@tree.com"
    , "nested" =:
      [ mkStrField "_id" "cc151c98-965d-4aed-82c1-a7c71f848250"
      , mkStrField "alias" "Mambo"
      ]
    ]

delRes3 :: Record
delRes3 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000005"
    , "email" =: Null
    , mkStrField "y" "2"
    ]

delRes4 :: Record
delRes4 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000005"
    , mkStrField "email" "blue@leaf.com"
    , mkStrField "y" "2"
    , "w" =: Null
    ]

delRes5 :: Record
delRes5 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000008"
    , mkStrField "email" "bamboo@tree.com"
    , "nested" =:
      [ mkStrField "_id" "cc151c98-965d-4aed-82c1-a7c71f848250"
      , "guid" =: Null
      , mkStrField "alias" "Mambo"
      ]
    ]

delRes6 :: Record
delRes6 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000008"
    , mkStrField "email" "bamboo@tree.com"
    , "nested" =:
      [ mkStrField "_id" "cc151c98-965d-4aed-82c1-a7c71f848250"
      , mkStrField "guid" "a01c793c-58d2-49d6-aba9-4012e8d6ace5"
      , mkStrField "alias" "Mambo"
      , "w" =: Null
      ]
    ]

-- | Exclude fields results

excludeRes1 :: Record
excludeRes1 = Record [mkStrField "_id" "584e58195984185eb8000005"]

excludeRes2 :: Record
excludeRes2 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000007"
    , "nested" =: [mkStrField "_id" "1234", mkStrField "name" "Alfred"]
    ]

-- | Merge records results

mergeRes1 :: Record
mergeRes1 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000006"
    , mkStrField "email" "tree@leaf.com"
    , mkStrField "y" "2"
    , mkStrField "x" "1"
    ]

mergeRes2 :: Record
mergeRes2 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000008"
    , mkStrField "email" "bamboo@tree.com"
    , "nested" =:
      [ mkStrField "_id" "cc151c98-965d-4aed-82c1-a7c71f848250"
      , mkStrField "guid" "a01c793c-58d2-49d6-aba9-4012e8d6ace5"
      , mkStrField "name" "Alfred"
      , mkStrField "alias" "Mambo"
      ]
    ]

-- Populate defaults results

popRes1 =
  Record
    [ mkStrField "_id" "31889d94-dada-481d-a7d8-0b5e48ee54f3"
    , mkBoolField "admin" False
    , mkIntField "count" 1
    , mkBoolField "disabled" False
    ]

-- Set field results

setRes1 :: Record
setRes1 =
  Record
    [ mkStrField "_id" "123"
    , mkStrField "email" "blue@leaf.com"
    , mkStrField "y" "2"
    ]

setRes2 :: Record
setRes2 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000005"
    , mkStrField "email" "blue@leaf.com"
    , mkStrField "y" "2"
    , mkStrField "x" "3"
    ]

setRes3 :: Record
setRes3 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000008"
    , mkStrField "email" "bamboo@tree.com"
    , "nested" =: [mkStrField "a" "1"]
    ]

setRes4 :: Record
setRes4 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000005"
    , mkStrField "email" "blue@leaf.com"
    , mkStrField "y" "2"
    , "nested" =: ["a" =: ["b" =: [mkStrField "c" "1"]]]
    ]

setRes5 :: Record
setRes5 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000008"
    , mkStrField "email" "bamboo@tree.com"
    , "nested" =:
      [ mkStrField "_id" "cc151c98-965d-4aed-82c1-a7c71f848250"
      , mkStrField "guid" "a01c793c-58d2-49d6-aba9-4012e8d6ace5"
      , mkStrField "alias" "Mambo"
      , mkStrField "a" "1"
      ]
    ]

setRes6 :: Record
setRes6 =
  Record
    [ mkStrField "_id" "584e58195984185eb8000008"
    , mkStrField "email" "bamboo@tree.com"
    , "nested" =:
      [ mkStrField "_id" "cc151c98-965d-4aed-82c1-a7c71f848250"
      , mkStrField "guid" "1"
      , mkStrField "alias" "Mambo"
      ]
    ]

-- Validate results

valRes1 :: ValidationResult
valRes1 =
  ValidationErrors
    [ mkStrField "count" "Field is required"
    , mkStrField "name" "Field is required"
    , mkStrField "x" "Field is not allowed"
    ]
