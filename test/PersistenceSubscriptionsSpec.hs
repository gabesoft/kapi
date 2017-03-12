{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Tests for Persistence.Xandar.Subscriptions
module Main (main) where

import Control.Monad.IO.Class
import Data.Bson
import Persistence.Common
import Persistence.Facade (mkIdIndexedMap)
import Persistence.Xandar.Subscriptions
import Persistence.Xandar.UserPosts (mkUserPostId)
import Test.Hspec
import TestHelper
import Types.Common
import Util.Constants

main :: IO ()
main =
  hspec $
  describe "Persistence.Xandar.Subscriptions" $ do
    describe "mkSubscriptions" $ do
      it "should use input title if exists" verifyInputOverridesTitle
      it "should use feed title if none given" verifyMissingTitleUsesFeed

    describe "addTags" $
      it "populates the tags field" verifyAddTags

    describe "mkUserPostOnSubUpdate" $
      it "creates a correct user post" =<< runIO verifyMkUserPostOnSubUpdate

verifyAddTags :: Expectation
verifyAddTags = expected `shouldMatchList` actualTags
  where
    actualTags :: [String]
    actualTags = getValue' "tags" actual
    actual =
      addTags
        (mkRecord old)
        (mkRecord new)
        (Just $ mkRecord existing)
        emptyRecord
    old = ["a", "b", "c"]
    new = ["x", "b", "c", "e"]
    existing = ["x", "a", "y", "w"]
    expected = ["x", "b", "c", "e", "y", "w"]
    mkRecord t = Record [mkStrListField "tags" t]

verifyMkUserPostOnSubUpdate :: MonadIO m => m Expectation
verifyMkUserPostOnSubUpdate = do
  result <- mkUserPostOnSubUpdate oldSubMap newSubMap oldMap post
  return $ replaceDates (fst $ fromRight result) `shouldMatchRecord` newUserPost
  where
    oldSubMap = mkFeedIndexedMap [oldSub]
    newSubMap = mkFeedIndexedMap [newSub]
    oldMap = mkIdIndexedMap [oldUserPost]

verifyInputOverridesTitle :: Expectation
verifyInputOverridesTitle =
  mkSubscriptions [feed] [input1] `shouldMatchRecords` [input1]

verifyMissingTitleUsesFeed :: Expectation
verifyMissingTitleUsesFeed =
  mkSubscriptions [feed] [input2] `shouldMatchRecords` [result2]

shouldMatchRecords :: [Record] -> [Record] -> Expectation
shouldMatchRecords xs ys = head xs `shouldMatchRecord` head ys

replaceDates :: Record -> Record
replaceDates = replaceUTCDate updatedAtLabel . replaceUTCDate createdAtLabel

oldSub :: Record
oldSub =
  Record
    [ mkTxtField "feedId" feedId
    , mkRecId subId
    , mkTxtField "userId" userId
    , mkStrListField "tags" ["a", "b", "c"]
    ]

newSub :: Record
newSub =
  Record
    [ mkTxtField "feedId" feedId
    , mkRecId subId
    , mkTxtField "userId" userId
    , mkStrListField "tags" ["a", "x", "y"]
    ]

oldUserPost :: Record
oldUserPost =
  Record
    [ mkRecId (mkUserPostId subId postId)
    , mkTxtField "feedId" feedId
    , mkTxtField "postId" postId
    , mkStrListField "tags" ["a", "b", "c", "k", "m"]
    ]

newUserPost :: Record
newUserPost =
  Record
    [ mkTxtField "feedId" feedId
    , mkTxtField "postId" postId
    , mkStrListField "tags" ["a", "k", "m", "x", "y"]
    , createdAtLabel =: date dateReplacement
    , updatedAtLabel =: date dateReplacement
    ]

post :: Record
post = Record [mkRecId postId, mkTxtField "feedId" feedId]

input1 :: Record
input1 =
  Record
    [ mkTxtField "title" "Input title"
    , mkTxtField "feedId" feedId
    , mkTxtField "userId" userId
    ]

input2 :: Record
input2 = Record [mkTxtField "feedId" feedId, mkTxtField "userId" userId]

feedId :: RecordId
feedId = "56d7de07c788cb1d6eb91a82"

userId :: RecordId
userId = "56d7de07c788cb1d6eb91a81"

postId :: RecordId
postId = "56d7de07c788cb1d6eb91a83"

subId :: RecordId
subId = "56d7de07c788cb1d6eb91a84"

feed :: Record
feed = Record [mkTxtField "title" "Feed title", mkRecId feedId]

result2 :: Record
result2 =
  Record
    [ mkTxtField "title" "Feed title"
    , mkTxtField "feedId" feedId
    , mkTxtField "userId" userId
    ]