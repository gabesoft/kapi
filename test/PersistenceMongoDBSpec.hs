{-# LANGUAGE OverloadedStrings #-}

-- |
-- Tests for Persistence.MongoDB
module Main
  ( main
  ) where

import Data.Bson
import qualified Data.Map.Strict as Map
import Persistence.Common
import Persistence.MongoDB
import Test.Hspec
import Types.Common

main =
  hspec $
  describe "Persistence.MongoDB" $
  do it "validates that a record has a valid id - missing" $
       verifyValidateId rec1 res1
     it "validates that a record has a valid id - invalid" $
       verifyValidateId rec1 res1
     it "validates that a record has a valid id - valid" $
       verifyValidateId rec3 (ValidationErrors [])

verifyValidateId r exp = snd (validateHasId r) `shouldBe` exp

rec1 :: Record
rec1 = Record ["email" =: ("a@email.com" :: String)]

rec2 :: Record
rec2 = Record ["_id" =: ("123" :: String)]

rec3 :: Record
rec3 = Record ["_id" =: ("586763745984183aef000002" :: String)]

res1 :: ValidationResult
res1 = ValidationErrors ["_id" =: ("Field is required" :: String)]
