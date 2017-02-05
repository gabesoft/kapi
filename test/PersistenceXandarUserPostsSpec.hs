{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Tests for Persistence.Xandar.UserPosts
module Main
  ( main
  ) where

import Data.Bson
import Data.List (all)
import qualified Data.Map.Strict as Map
import Mocks.Common
import Network.HTTP.Types
import Persistence.Common
import Persistence.Xandar.UserPosts
import Test.Hspec
import Test.QuickCheck
import TestHelper
import Types.Common

main :: IO ()
main =
  hspec $
  describe "Persistence.Xandar.UserPosts" $
  do it "ensures a user post contains a postId and a subscriptionId" $
       verifyValidation inputInvalid1 (inputInvalid1, invalidErrors)
     it "creates a user post" $
       verifyMakeUserPost inputValid (userPost1, userPost1Id)
     it "can handle multiple posts" $
       verifyMakeUserPosts [inputValid] [Right (userPost1, userPost1Id)]
     it "returns results and errors" $
       verifyMakeUserPosts [inputValid, inputInvalid2] result1

verifyValidation :: Record -> (Record, ValidationResult) -> Expectation
verifyValidation input exp = validate userPostDefinition input `shouldBe` exp

verifyMakeUserPost :: Record -> (Record, RecordId) -> Expectation
verifyMakeUserPost input exp = mkUserPost (subscription, post) input `shouldBe` exp

verifyMakeUserPosts :: [Record] -> [Either ApiError (Record, RecordId)] -> Expectation
verifyMakeUserPosts input exp =
  mkUserPosts ([subscription], [post]) input `shouldBe` exp

invalidErrors :: ValidationResult
invalidErrors =
  ValidationErrors
    [ mkStrField "postId" "Field is required"
    , mkStrField "subscriptionId" "Field is required"
    ]

inputInvalid1 :: RecordData Field
inputInvalid1 =
  Record
    [ "post" =:
      [ mkIntField "version" 0
      , mkStrField "guid" "27d179f7-0c55-4177-9f15-404221e9b8c0"
      ]
    , mkStrField "feedId" "57e9f802d5ec56510904c9d9"
    ]

inputInvalid2 :: RecordData Field
inputInvalid2 =
  Record
    [ mkBoolField "read" True
    , mkStrField "title" "random title"
    , mkStrField "subscriptionId" "56d7de07c788cb1d6eb91a82"
    , mkStrField "postId" "56d7d88bc788cb1d6eb919c1"
    , "post" =: [mkStrField "author" "unknown"]
    , mkStrListField "tags" ["haskell", "javascript"]
    ]

inputValid :: RecordData Field
inputValid =
  Record
    [ mkBoolField "read" True
    , mkStrField "title" "random title"
    , mkStrField "subscriptionId" "56d7de07c788cb1d6eb91a82"
    , mkStrField "postId" "56d7d88bc788cb1d6eb919a1"
    , mkStrField "feedId" "to be overwritten"
    , mkStrField "userId" "to be overwritten"
    , "post" =: [mkStrField "author" "unknown"]
    , mkStrListField "tags" ["haskell", "javascript"]
    ]

userPost1 :: RecordData Field
userPost1 =
  Record
    [ mkStrListField "tags" ["haskell", "javascript"]
    , mkStrField "title" "random title"
    , mkBoolField "read" True
    , mkStrField "subscriptionId" "56d7de07c788cb1d6eb91a82"
    , mkStrField "postId" "56d7d88bc788cb1d6eb919a1"
    , mkStrField "feedId" "56d7de07c788cb1d6eb91a6d"
    , mkStrField "userId" "56d7cc3fccee17506699e735"
    , "post" =:
      [ mkStrField "author" "Post author"
      , mkStrField "comments" []
      , mkStrField "date" "2016-03-01T03:45:00.000Z"
      , mkStrField "description" "Post description"
      , mkStrField "guid" "8196e7a1-4fdd-417d-b49b-c74640089314"
      , mkStrField "link" "http://example.com/feeds/1233.html"
      , mkStrField "summary" "Post summary"
      , mkStrField "title" "Post title"
      , "image" =: [mkStrField "url" "http://example.com/feeds/1233.rss"]
      ]
    ]

result1 :: [Either ApiError (RecordData Field, RecordId)]
result1 =
  [ Right (userPost1, userPost1Id)
  , Left
      ApiError
      { apiErrorStatus =
        Status
        { statusCode = 400
        , statusMessage = "Bad Request"
        }
      , apiErrorMessage =
        "Post 56d7d88bc788cb1d6eb919c1 not found."
      }
  ]

userPost1Id :: RecordId
userPost1Id = "56d7de07c788cb1d6eb91a82-56d7d88bc788cb1d6eb919a1"

subscription :: RecordData Field
subscription =
  Record
    [ mkStrField "createdAt" "2016-03-03T06:47:35.463Z"
    , mkBoolField "disabled" False
    , mkStrField "feedId" "56d7de07c788cb1d6eb91a6d"
    , mkStrField "_id" "56d7de07c788cb1d6eb91a82"
    , mkStrListField "tags" ["programming"]
    , mkStrField "title" "Feed title"
    , mkStrField "updatedAt" "2016-03-03T07:05:56.533Z"
    , mkStrField "userId" "56d7cc3fccee17506699e735"
    ]

post :: RecordData Field
post =
  Record
    [ mkStrField "author" "Post author"
    , mkStrField "comments" []
    , mkStrField "date" "2016-03-01T03:45:00.000Z"
    , mkStrField "description" "Post description"
    , mkStrField "feedId" "56d7de07c788cb1d6eb91a6d"
    , mkStrField "guid" "8196e7a1-4fdd-417d-b49b-c74640089314"
    , mkStrField "_id" "56d7d88bc788cb1d6eb919a1"
    , mkStrField "link" "http://example.com/feeds/1233.html"
    , mkStrField "pubdate" "2016-03-01T03:45:00.000Z"
    , mkStrField "summary" "Post summary"
    , mkStrField "title" "Post title"
    , "image" =: [mkStrField "url" "http://example.com/feeds/1233.rss"]
    ]
