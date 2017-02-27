{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Tests for Persistence.Xandar.Subscriptions
module Main (main) where

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
import Persistence.Xandar.Subscriptions
import Persistence.Xandar.UserPosts
import Test.Hspec
import Test.QuickCheck
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
    describe "addTags" $ do
      it "populates the tags field" verifyAddTags

verifyAddTags = expected `shouldMatchList` actualTags
  where
    actualTags :: [String]
    actualTags = getValue' "tags" actual
    actual = addTags (mkRecord old) (mkRecord new) (Just $ mkRecord existing) emptyRecord
    old = ["a", "b", "c"]
    new = ["x", "b", "c", "e"]
    existing = ["x", "a", "y", "w"]
    expected = ["x", "b", "c", "e", "y", "w"]
    mkRecord t = Record [mkStrListField "tags" t]

verifyInputOverridesTitle :: Expectation
verifyInputOverridesTitle =
  mkSubscriptions [feed] [input1] `shouldMatchRecords` [input1]

verifyMissingTitleUsesFeed :: Expectation
verifyMissingTitleUsesFeed =
  mkSubscriptions [feed] [input2] `shouldMatchRecords` [result2]

shouldMatchRecords :: [Record] -> [Record] -> Expectation
shouldMatchRecords xs ys = head xs `shouldMatchRecord` head ys

input1 :: Record
input1 =
  Record
    [ mkTxtField "title" "Input title"
    , mkTxtField "feedId" feedId
    , mkTxtField "userId" userId
    ]

input2 :: Record
input2 = Record [mkTxtField "feedId" feedId, mkTxtField "userId" userId]

feedId = "56d7de07c788cb1d6eb91a82"
userId = "56d7de07c788cb1d6eb91a81"

feed :: Record
feed =
  Record [mkTxtField "title" "Feed title", mkRecId feedId]

result2 :: Record
result2 =
  Record
    [ mkTxtField "title" "Feed title"
    , mkTxtField "feedId" feedId
    , mkTxtField "userId" userId
    ]
