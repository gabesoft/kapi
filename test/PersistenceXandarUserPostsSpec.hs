{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Tests for Persistence.Xandar.UserPosts
module Main (main) where

import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Bson
import Data.List (all)
import qualified Data.Map.Strict as Map
import Mocks.Common
import Network.HTTP.Types
import Persistence.Common
import Persistence.Xandar.Common
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
     it "can replace a user post" =<<
       runIO (verifyMakeUserPost True inputValid1 (userPost1, userPost1Id))
     it "can modify a user post" =<<
       runIO (verifyMakeUserPost False inputValid2 (userPost2, userPost1Id))
     it "ensures the subscription and post have the same feed id" =<< runIO verifyFeedIdCheck
     it "can handle multiple posts" =<<
       runIO (verifyMakeUserPosts [inputValid1] [Right (userPost1, userPost1Id)])
     it "returns results and errors" =<<
       runIO (verifyMakeUserPosts [inputValid1, inputInvalid2] result1)

verifyValidation :: Record -> (Record, ValidationResult) -> Expectation
verifyValidation input exp = validateRecord userPostDefinition input `shouldBe` exp

verifyMakeUserPost
  :: MonadIO m
  => Bool -> Record -> (Record, RecordId) -> m Expectation
verifyMakeUserPost replace input exp = do
  actual <- mkUserPost replace (Just sub1, Just post) (Just existingPost, input)
  return $ replaceDates (fromRight actual) `shouldBe` exp

verifyFeedIdCheck
  :: MonadIO m
  => m Expectation
verifyFeedIdCheck = do
  actual <- mkUserPost True (Just sub2, Just post) (Nothing, inputValid1)
  return $ actual `shouldBe` result2

verifyMakeUserPosts
  :: (MonadIO m)
  => [Record] -> [Either ApiError (Record, RecordId)] -> m Expectation
verifyMakeUserPosts input exp = do
  actual <- mkUserPosts True ([sub1], [post]) ([existingPost], input)
  return $ (fmap replaceDates <$> actual) `shouldBe` exp

replaceDates :: (Record, c) -> (Record, c)
replaceDates = first (replaceDate updatedAtLabel)

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
    , mkStrField "title" "Feed title"
    , mkStrField "subscriptionId" "56d7de07c788cb1d6eb91a82"
    , mkStrField "postId" "56d7d88bc788cb1d6eb919c1"
    , "post" =: [mkStrField "author" "unknown"]
    , mkStrListField "tags" ["haskell", "javascript"]
    ]

inputValid1 :: RecordData Field
inputValid1 =
  Record
    [ mkBoolField "read" True
    , mkStrField "title" "Feed title"
    , mkStrField "subscriptionId" "56d7de07c788cb1d6eb91a82"
    , mkStrField "postId" "56d7d88bc788cb1d6eb919a1"
    , mkStrField "feedId" "to be overwritten"
    , mkStrField "userId" "to be overwritten"
    , "post" =: [mkStrField "author" "unknown"]
    , mkStrListField "tags" ["haskell", "javascript"]
    ]

inputValid2 :: RecordData Field
inputValid2 =
  Record
    [ mkBoolField "read" True
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
    , mkStrField "title" "Feed title"
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
    , mkStrField "postId" "56d7d88bc788cb1d6eb919a1"
    , mkStrField "subscriptionId" "56d7de07c788cb1d6eb91a82"
    , mkStrField "userId" "56d7cc3fccee17506699e735"
    , mkStrField "feedId" "56d7de07c788cb1d6eb91a6d"
    , mkBoolField "read" True
    , mkTxtField updatedAtLabel "12345"
    , mkTxtField createdAtLabel "2016-09-27T04:39:31.460Z"
    ]

userPost2 :: RecordData Field
userPost2 =
  Record
    [ mkStrListField "tags" ["haskell", "javascript"]
    , mkStrField "title" "Existing title"
    , "post" =:
      [ mkStrField "author" "Existing post author"
      , mkStrField "comments" []
      , mkStrField "date" "2016-03-01T03:45:00.000Z"
      , mkStrField "description" "Existing post description"
      , mkStrField "guid" "8196e7a1-4fdd-417d-b49b-c74640089314"
      , mkStrField "link" "http://example.com/feeds/1233.html"
      , mkStrField "summary" "Post summary"
      , mkStrField "title" "Post title"
      , "image" =: [mkStrField "url" "http://example.com/feeds/1233.rss"]
      ]
    , mkStrField "postId" "56d7d88bc788cb1d6eb919a1"
    , mkStrField "subscriptionId" "56d7de07c788cb1d6eb91a82"
    , mkStrField "userId" "56d7cc3fccee17506699e735"
    , mkStrField "feedId" "56d7de07c788cb1d6eb91a6d"
    , mkBoolField "read" True
    , mkTxtField updatedAtLabel "12345"
    , mkTxtField createdAtLabel "2016-09-27T04:39:31.460Z"
    ]

existingPost :: RecordData Field
existingPost =
  Record
    [ mkStrListField "tags" ["existing-tag", "javascript"]
    , mkStrField "title" "Existing title"
    , mkRecId userPost1Id
    , "post" =:
      [ mkStrField "author" "Existing post author"
      , mkStrField "comments" []
      , mkStrField "date" "2016-03-01T03:45:00.000Z"
      , mkStrField "description" "Existing post description"
      , mkStrField "guid" "8196e7a1-4fdd-417d-b49b-c74640089314"
      , mkStrField "link" "http://example.com/feeds/1233.html"
      , mkStrField "summary" "Post summary"
      , mkStrField "title" "Post title"
      , "image" =: [mkStrField "url" "http://example.com/feeds/1233.rss"]
      ]
    , mkStrField "postId" "56d7d88bc788cb1d6eb919a1"
    , mkStrField "subscriptionId" "56d7de07c788cb1d6eb91a82"
    , mkStrField "userId" "56d7cc3fccee17506699e735"
    , mkStrField "feedId" "56d7de07c788cb1d6eb91a6d"
    , mkBoolField "read" False
    , mkTxtField updatedAtLabel "2016-09-27T04:53:11.163Z"
    , mkTxtField createdAtLabel "2016-09-27T04:39:31.460Z"
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
        "Post not found. Original input: {\"read\":true,\"post\":{\"author\":\"unknown\"},\"postId\":\"56d7d88bc788cb1d6eb919c1\",\"title\":\"Feed title\",\"subscriptionId\":\"56d7de07c788cb1d6eb91a82\",\"tags\":[\"haskell\",\"javascript\"]}"
      }
  ]

result2 :: Either ApiError b
result2 =
  Left
    ApiError
    { apiErrorStatus =
      Status
      { statusCode = 400
      , statusMessage = "Bad Request"
      }
    , apiErrorMessage =
      "Post belongs to a different subscription. Original input: {\"read\":true,\"post\":{\"author\":\"unknown\"},\"userId\":\"to be overwritten\",\"feedId\":\"to be overwritten\",\"postId\":\"56d7d88bc788cb1d6eb919a1\",\"title\":\"Feed title\",\"subscriptionId\":\"56d7de07c788cb1d6eb91a82\",\"tags\":[\"haskell\",\"javascript\"]}"
    }

userPost1Id :: RecordId
userPost1Id = "56d7de07c788cb1d6eb91a82-56d7d88bc788cb1d6eb919a1"

sub1 :: RecordData Field
sub1 =
  Record
    [ mkStrField "createdAt" "2016-03-03T06:47:35.463Z"
    , mkBoolField "disabled" False
    , mkStrField "feedId" "56d7de07c788cb1d6eb91a6d"
    , mkStrField "_id" "56d7de07c788cb1d6eb91a82"
    , mkStrListField "tags" ["programming"]
    , mkStrField "title" "Feed title"
    , mkTxtField "updatedAt" "2016-03-03T07:05:56.533Z"
    , mkStrField "userId" "56d7cc3fccee17506699e735"
    ]

sub2 :: RecordData Field
sub2 =
  Record
    [ mkStrField "createdAt" "2016-03-03T06:47:35.463Z"
    , mkBoolField "disabled" False
    , mkStrField "feedId" "56d7de07c788cb1d6eb91a6e"
    , mkStrField "_id" "56d7de07c788cb1d6eb91a82"
    , mkStrListField "tags" ["programming"]
    , mkStrField "title" "Feed title"
    , mkTxtField "updatedAt" "2016-03-03T07:05:56.533Z"
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
