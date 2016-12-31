{-# LANGUAGE OverloadedStrings #-}

-- |
-- Tests for Persistence.MongoDB
module Main
  ( main
  ) where

import Control.Monad.IO.Class
import Data.Bson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time
import Persistence.Common
import Persistence.MongoDB
import Test.Hspec
import Test.Hspec.Core.Spec
import TestHelper
import Types.Common

main :: IO ()
main =
  hspec $
  describe "Persistence.MongoDB" $
  do it "validates that a record has a valid id - missing" $
       verifyValidateId rec1 res1
     it "validates that a record has a valid id - invalid" $
       verifyValidateId rec1 res1
     it "validates that a record has a valid id - valid" $
       verifyValidateId rec3 (ValidationErrors [])
     it "creates a document ready to be saved - missing id" =<<
       runIO (verifyMkInDocument rec4 res2)
     it "creates a document ready to be saved - invalid id" =<<
       runIO (verifyMkInDocument rec6 res2)
     it "creates a document ready to be saved - valid id" =<<
       runIO (verifyMkInDocument rec7 res4)
     it "creates a record ready to be returned" =<<
       runIO (verifyMkOutDocument rec5 res3)

verifyValidateId r exp = snd (validateHasId r) `shouldBe` exp

verifyMkInDocument
  :: MonadIO m
  => Record -> Record -> m Expectation
verifyMkInDocument r exp = do
  doc <- mkInDocument r
  return $ modDate "_updatedAt" (Record doc) `shouldBe` exp

verifyMkOutDocument
  :: MonadIO m
  => Document -> Record -> m Expectation
verifyMkOutDocument doc exp = do
  r <- mkOutRecord doc
  return $ modDate "_createdAt" r `shouldBe` exp

rec1 :: Record
rec1 = Record [mkStrField "email" "a@email.com"]

rec2 :: Record
rec2 = Record [mkRecId "123"]

rec3 :: Record
rec3 = Record [mkRecId "586763745984183aef000002"]

rec4 :: Record
rec4 = Record [mkStrField "email" "a@e.com", mkStrField "_createdAt" "1234"]

rec5 :: Document
rec5 = [mkObjId "586763745984183aef000002"]

rec6 :: Record
rec6 = Record [mkStrField "email" "a@e.com", mkRecId "1234"]

rec7 :: Record
rec7 = Record [mkStrField "email" "a@e.com", mkRecId "586763745984183aef000002"]

res1 :: ValidationResult
res1 = ValidationErrors [mkRecId "Field is required"]

res2 :: RecordData Field
res2 = Record [mkStrField "email" "a@e.com", mkStrField "_updatedAt" "12345"]

res3 :: RecordData Field
res3 =
  Record [mkRecId "586763745984183aef000002", mkStrField "_createdAt" "12345"]

res4 :: RecordData Field
res4 =
  Record
    [ mkStrField "email" "a@e.com"
    , mkObjId "586763745984183aef000002"
    , mkStrField "_updatedAt" "12345"
    ]
