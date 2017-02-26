{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Tests for Persistence.Xandar.UserPosts
module Main
  ( main
  ) where

import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Bson
import Data.List (all)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (UTCTime)
import Mocks.Common
import Network.HTTP.Types
import Persistence.Common
import Persistence.Xandar.Common
import Persistence.Xandar.UserPosts
import Test.Hspec
import Test.QuickCheck
import TestHelper
import Types.Common
import Util.Constants

main :: IO ()
main =
  hspec $
  describe "Persistence.Xandar.UserPosts" $ do
    describe "validate" $ do
      it "ensures a user post contains a postId and a subscriptionId" $
        verifyValidation inputInvalid1 (inputInvalid1, invalidErrors)

    describe "createUserPost" $ do
      it "missing post results in an error" =<<
        runIO verifyCreateUserPostMissingPost
      it "missing subscription results in an error" =<<
        runIO verifyCreateUserPostMissingSub
      it "checks that the subscription and post have the same feed id" =<<
        runIO verifyCreateUserPostMismatch
      it "generates the correct id" =<< runIO verifyCreateUserPostId
      it "generates the correct record" =<< runIO verifyCreateUserPostRecord

    describe "replaceUserPost" $ do
      it "removes the id field" =<< runIO (verifyExcludesId replaceUserPost)
      it "keeps the createdAt text value" =<< runIO (verifyCreatedAtText replaceUserPost)
      it "keeps the createdAt UTC value" =<< runIO (verifyCreatedAtUTC replaceUserPost)
      it "replaces the existing record" =<< runIO verifyReplace

    describe "modifyUserPost" $ do
      it "removes the id field" =<< runIO (verifyExcludesId modifyUserPost)
      it "keeps the createdAt text value" =<< runIO (verifyCreatedAtText modifyUserPost)
      it "keeps the createdAt UTC value" =<< runIO (verifyCreatedAtUTC modifyUserPost)
      it "modifies the existing record" =<< runIO verifyModify

verifyValidation :: Record -> (Record, ValidationResult) -> Expectation
verifyValidation input exp =
  validateRecord userPostDefinition input `shouldBe` exp

verifyCreateUserPostMissingPost
  :: MonadIO m
  => m Expectation
verifyCreateUserPostMissingPost = do
  actual <- createUserPost (Just sub1) Nothing emptyInput
  return $ actual `shouldBe` Left missingPostResult

verifyCreateUserPostMissingSub
  :: MonadIO m
  => m Expectation
verifyCreateUserPostMissingSub = do
  actual <- createUserPost Nothing (Just post) emptyInput
  return $ actual `shouldBe` Left missingSubResult

verifyCreateUserPostMismatch
  :: MonadIO m
  => m Expectation
verifyCreateUserPostMismatch = do
  actual <- createUserPost (Just sub2) (Just post) inputValid1
  return $ actual `shouldBe` result2 (Just inputValid1)

verifyCreateUserPostId
  :: MonadIO m
  => m Expectation
verifyCreateUserPostId = do
  actual <- createUserPost (Just sub1) (Just post) inputValid1
  let rid = getIdValue' sub1 <> "-" <> getIdValue' post
  return $ snd (fromRight actual) `shouldBe` rid

verifyCreateUserPostRecord
  :: MonadIO m
  => m Expectation
verifyCreateUserPostRecord = do
  actual <- createUserPost (Just sub1) (Just post) inputValid1
  return $ replaceDates (getRecord actual) `shouldBe` replaceDates userPost1

verifyExcludesId
  :: MonadIO m
  => (Maybe Record -> Record -> m (Either ApiError (Record, RecordId)))
  -> m Expectation
verifyExcludesId f = do
  actual <- f (Just userPost1) inputValid2
  return $ hasField idLabel (getRecord actual) `shouldBe` False

verifyCreatedAt
  :: (Monad m, Show a, Eq a)
  => (Record -> a)
  -> Record
  -> (Maybe Record -> RecordData Field -> m (Either ApiError (Record, RecordId)))
  -> m Expectation
verifyCreatedAt getCreatedAt record f = do
  actual <- f (Just record) inputValidWithDates
  let createdAtActual = getCreatedAt (getRecord actual)
  let createdAtExpected = getCreatedAt record
  return $ createdAtActual `shouldBe` createdAtExpected

verifyCreatedAtText
  :: (Maybe Record -> RecordData Field -> IO (Either ApiError (Record, RecordId)))
  -> IO Expectation
verifyCreatedAtText = verifyCreatedAt createdAtText userPost2

verifyCreatedAtUTC
  :: (Maybe Record -> RecordData Field -> IO (Either ApiError (Record, RecordId)))
  -> IO Expectation
verifyCreatedAtUTC = verifyCreatedAt createdAtUTC userPost1

createdAtText :: Record -> Text
createdAtText = getValue' createdAtLabel

createdAtUTC :: Record -> UTCTime
createdAtUTC = getValue' createdAtLabel

verifyReplace :: MonadIO m => m Expectation
verifyReplace = do
  actual <- replaceUserPost (Just userPost1) inputValidForReplace
  return $ replaceUpdatedAt (getRecord actual) `shouldBe` resultForReplace

verifyModify :: MonadIO m => m Expectation
verifyModify = do
  actual <- modifyUserPost (Just userPost2) inputValidForModify
  return $ replaceUpdatedAt (getRecord actual) `shouldBe` resultForModify

getRecord :: Either ApiError (Record, RecordId) -> Record
getRecord = fst . fromRight

emptyInput :: RecordData a
emptyInput = Record []

missingSubResult :: ApiError
missingSubResult =
  ApiError
    (Just emptyInput)
    status404
    "Record not found in subscriptions collection."

missingPostResult :: ApiError
missingPostResult =
  ApiError (Just emptyInput) status404 "Record not found in posts collection."

replaceDates :: Record -> Record
replaceDates = replaceUTCDate updatedAtLabel . replaceUTCDate createdAtLabel

replaceUpdatedAt :: Record -> Record
replaceUpdatedAt = replaceUTCDate updatedAtLabel

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

inputValidWithDates :: RecordData Field
inputValidWithDates =
  Record
    [ mkBoolField "read" True
    , mkStrField "subscriptionId" "56d7de07c788cb1d6eb91a82"
    , mkStrField "postId" "56d7d88bc788cb1d6eb919a1"
    , mkStrField "feedId" "to be overwritten"
    , mkStrField "userId" "to be overwritten"
    , "post" =: [mkStrField "author" "unknown"]
    , mkStrListField "tags" ["haskell", "javascript"]
    , updatedAtLabel =: date "2016-02-23T01:21:15.513Z"
    , createdAtLabel =: date "2016-01-23T01:21:15.513Z"
    ]

inputValidForReplace :: RecordData Field
inputValidForReplace =
  Record
    [ mkBoolField "read" False
    , mkStrField "title" "Replaced title"
    , mkStrField "subscriptionId" "56d7de07c788cb1d6eb91a82"
    , mkStrField "postId" "56d7d88bc788cb1d6eb919a1"
    , mkStrField "feedId" "to be overwritten"
    , mkStrField "userId" "to be overwritten"
    , "post" =: [mkStrField "author" "unknown"]
    , mkStrListField "tags" ["haskell", "javascript"]
    ]

resultForReplace :: RecordData Field
resultForReplace =
  Record
    [ mkStrListField "tags" ["haskell", "javascript"]
    , mkStrField "title" "Replaced title"
    , "post" =: [mkStrField "author" "unknown"]
    , mkStrField "postId" "56d7d88bc788cb1d6eb919a1"
    , mkStrField "subscriptionId" "56d7de07c788cb1d6eb91a82"
    , mkStrField "userId" "56d7cc3fccee17506699e735"
    , mkStrField "feedId" "56d7de07c788cb1d6eb91a6d"
    , mkBoolField "read" False
    , updatedAtLabel =: date dateReplacement
    , createdAtLabel =: date "2016-09-27T04:39:31.460Z"
    ]

inputValidForModify :: RecordData Field
inputValidForModify =
  Record
    [ mkBoolField "read" False
    , mkStrField "title" "Modified title"
    , mkStrField "subscriptionId" "56d7de07c788cb1d6eb91a82"
    , mkStrField "postId" "56d7d88bc788cb1d6eb919a1"
    , mkStrField "feedId" "to be overwritten"
    , mkStrField "userId" "to be overwritten"
    , "post" =: [mkStrField "author" "Modified post author"]
    , mkStrListField "tags" ["tech"]
    ]

resultForModify :: RecordData Field
resultForModify =
  Record
    [ mkStrListField "tags" ["tech"]
    , mkStrField "title" "Modified title"
    , "post" =:
      [ mkStrField "author" "Modified post author"
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
    , updatedAtLabel =: date dateReplacement
    , mkTxtField createdAtLabel "2016-09-27T04:39:31.460Z"
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
    , updatedAtLabel =: date "2016-09-27T04:53:11.163Z"
    , createdAtLabel =: date "2016-09-27T04:39:31.460Z"
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
    , mkTxtField updatedAtLabel dateReplacement
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

result1 :: Maybe Record -> [Either ApiError (RecordData Field, RecordId)]
result1 input =
  [ Right (userPost1, userPost1Id)
  , Left
      ApiError
      { apiErrorInput = input
      , apiErrorStatus =
          Status {statusCode = 400, statusMessage = "Bad Request"}
      , apiErrorMessage = "Post not found."
      }
  ]

result2 :: Maybe Record -> Either ApiError b
result2 input =
  Left
    ApiError
    { apiErrorInput = input
    , apiErrorStatus = Status {statusCode = 400, statusMessage = "Bad Request"}
    , apiErrorMessage = "Post does not belong to subscription."
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
