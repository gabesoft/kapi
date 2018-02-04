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
import Persistence.Common
import Persistence.MongoDB
import Test.Hspec
import TestHelper
import Types.Common
import Util.Constants
import Data.Maybe

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
       runIO (verifyMkInDocument rec4 res5)
     it "creates a document ready to be saved - invalid id" =<<
       runIO (verifyMkInDocument rec6 res2)
     it "creates a document ready to be saved - valid id" =<<
       runIO (verifyMkInDocument rec7 res4)
     it "creates a document ready to be saved - date field" =<<
       runIO (verifyMkInDocument rec8 res8)
     it "creates a record ready to be returned" $ verifyMkOutDocument rec5 res3
     it "can make a sort field for ascending sort" $
       verifyMkSortField "email" (mkIntField "email" 1)
     it "can make a sort field for descending sort" $
       verifyMkSortField "-email" (mkIntField "email" (negate 1))
     it "converts a query to a document" $
       verifyQueryToDoc "(a eq \"b\") and (b gt 1) or (c:2 lt 2.4)" res6
     it "converts a query to a document - null" $
       verifyQueryToDoc "a eq null" ["a" =: Null]
     it "converts a query to a document - not null" $
       verifyQueryToDoc "a ~eq null" ["a" =: ["$ne" =: Null]]
     it "converts a query to a document - id field" $
       verifyQueryToDoc
         "foreignKey eq '586763745984183aef000002'"
         ["foreignKey" =: mkId "586763745984183aef000002"]
     it "converts a query to a document - id field 2" $
       verifyQueryToDoc
         "_id eq '586763745984183aef000002'"
         ["_id" =: mkId "586763745984183aef000002"]
     it "converts a query to a document - id field multiple" $
       verifyQueryToDoc
         "foreignKey in ['586763745984183aef000002','586763745984183aef000003']"
         queryRes1
     it "converts a query to a document - id field multiple 2" $
       verifyQueryToDoc
         "foreignKey in ['586763745984183aef000002', 586763745984183aef000003]"
         queryRes1
     it "converts a query to a document - id field multiple negated" $
       verifyQueryToDoc
         "foreignKey ~in ['586763745984183aef000002','586763745984183aef000003']"
         queryRes2
     it "converts a document to a record - new" $
       verifyDocumentToRecord recDef sampleNewDoc sampleNewRec
     it "converts a document to a record - old" $
       verifyDocumentToRecord recDef sampleDoc sampleRec
     it "converts a record to a document - new" $
       verifyRecordToDocument recDef sampleNewRec sampleNewDoc
     it "converts a record to a document - old" $
       verifyRecordToDocument recDef sampleRec sampleDoc
     it "converts a record to a document - invalid foreign key" $
       verifyRecordToDocument recDef sampleRecInvalidFK sampleDocMissingFK

verifyValidateId :: Record -> ValidationResult -> Expectation
verifyValidateId r expected = snd (validateRecordHasId r) `shouldBe` expected

verifyMkInDocument
  :: MonadIO m
  => Record -> Record -> m Expectation
verifyMkInDocument r expected = do
  doc <- mkInDocument recDef (not $ hasField "_id" r) r
  return $
    (replaceUTCDate createdAtLabel . replaceUTCDate updatedAtLabel) (Record doc) `shouldBe` expected

verifyMkOutDocument :: Document -> Record -> Expectation
verifyMkOutDocument doc expected =
  replaceUTCDate createdAtLabel (mkOutRecord recDef doc) `shouldBe` expected

verifyMkSortField :: Text -> Field -> Expectation
verifyMkSortField name expected = mkSortField name `shouldBe` Just expected

verifyQueryToDoc :: Text -> Document -> Expectation
verifyQueryToDoc query expected = queryToDoc recDef query `shouldBe` Right expected

verifyRecordToDocument :: RecordDefinition -> Record -> Document -> Expectation
verifyRecordToDocument def record expected =
  recordToDocument def record `shouldBe` expected

verifyDocumentToRecord :: RecordDefinition -> Document -> Record -> Expectation
verifyDocumentToRecord def document expected =
  documentToRecord def document `shouldBe` expected

recDef :: RecordDefinition
recDef =
  RecordDefinition "test-collection" mempty mempty $
  Map.fromList
    [ mkOptDef' "optional"
    , mkOptDef "optionalWithDefault" (1 :: Int)
    , mkReqDef' "required"
    , mkReqDef "requiredWithDefault" ("a" :: String)
    , mkIdDef "foreignKey"
    , mkDateDef' "someDate"
    ]

sampleId1 :: RecordId
sampleId1 = "586763745984183aef000002"

sampleId2 :: RecordId
sampleId2 = "586763745984183aef000004"

commonDoc :: Document
commonDoc =
  [ mkStrField "optional" "optionalValue"
  , mkIntField "optionalWithDefault" 1
  , mkIntField "required" 100
  , mkStrField "requiredWithDefault" "requiredValue"
  ]

sampleNewDoc :: Document
sampleNewDoc = merge commonDoc [mkObjId' "foreignKey" sampleId2]

sampleNewDocMissingFK :: Document
sampleNewDocMissingFK = commonDoc

sampleNewRec :: Record
sampleNewRec = Record (merge commonDoc [mkTxtField "foreignKey" sampleId2])

sampleNewRecInvalidFK :: Record
sampleNewRecInvalidFK = Record (merge commonDoc [mkStrField "foreignKey" "123"])

sampleDoc :: Document
sampleDoc = merge [mkObjId sampleId1] sampleNewDoc

sampleDocMissingFK :: Document
sampleDocMissingFK = merge [mkObjId sampleId1] sampleNewDocMissingFK

sampleRec :: Record
sampleRec = mergeRecords sampleNewRec (Record [mkRecId sampleId1])

sampleRecInvalidFK :: Record
sampleRecInvalidFK =
  mergeRecords sampleNewRecInvalidFK (Record [mkRecId sampleId1])

rec1 :: Record
rec1 = Record [mkStrField "email" "a@email.com"]

rec3 :: Record
rec3 = Record [mkRecId sampleId1]

rec4 :: Record
rec4 = Record [mkStrField "email" "a@e.com", mkStrField createdAtLabel "1234"]

rec5 :: Document
rec5 = [mkObjId sampleId1]

rec6 :: Record
rec6 = Record [mkStrField "email" "a@e.com", mkRecId "1234"]

rec7 :: Record
rec7 = Record [mkStrField "email" "a@e.com", mkRecId sampleId1]

rec8 :: Record
rec8 = Record [ mkRecId sampleId1, mkStrField "someDate" "2017-01-13T02:09:05.000Z" ]

res1 :: ValidationResult
res1 = ValidationErrors [mkRecId "Field is required"]

res2 :: RecordData Field
res2 =
  Record [mkStrField "email" "a@e.com", updatedAtLabel =: date dateReplacement]

res3 :: RecordData Field
res3 = Record [mkRecId sampleId1]

res4 :: RecordData Field
res4 =
  Record
    [ mkStrField "email" "a@e.com"
    , mkObjId sampleId1
    , updatedAtLabel =: date dateReplacement
    ]

res5 :: RecordData Field
res5 =
  Record
    [ mkStrField "email" "a@e.com"
    , createdAtLabel =: date dateReplacement
    , updatedAtLabel =: date dateReplacement
    ]

res6 :: Document
res6 =
  [ "$or" =:
    [ ["$and" =: [[mkStrField "a" "b"], ["b" =: [mkIntField "$gt" 1]]]]
    , ["c" =: [mkFloatField "$lt" 2.4]]
    ]
  ]

res8 :: RecordData Field
res8 =
  Record
    [ mkObjId sampleId1
    , "someDate" =: date "2017-01-13T02:09:05.000Z"
    , updatedAtLabel =: date dateReplacement
    ]

queryRes1 :: Document
queryRes1 =
  [ "foreignKey" =:
    [ "$in" =:
      [mkId "586763745984183aef000002", mkId "586763745984183aef000003"]
    ]
  ]

queryRes2 :: Document
queryRes2 =
  [ "foreignKey" =:
    [ "$nin" =:
      [mkId "586763745984183aef000002", mkId "586763745984183aef000003"]
    ]
  ]
