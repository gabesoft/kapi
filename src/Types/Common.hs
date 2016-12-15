{-# LANGUAGE DeriveGeneric #-}

-- | Common types
module Types.Common where

import Data.Aeson
import Data.AesonBson (aesonify, bsonify)
import Data.Bson
import Data.Text (Text)
import GHC.Generics

type RecordId = Text

data Record =
  Record [Field]
  deriving (Eq, Show)

instance ToJSON Record where
  toJSON (Record r) = Object (aesonify r)

instance FromJSON Record where
  parseJSON (Object obj) = return $ Record (bsonify obj)
  parseJSON _ = fail "empty"

data ApiError = ApiError
  { _message :: String
  } deriving (Eq, Show, Generic)

instance ToJSON ApiError

data ModelOrError a
  = Fail ApiError
  | Succ a

instance (ToJSON a) =>
         ToJSON (ModelOrError a) where
  toJSON (Fail e) = toJSON e
  toJSON (Succ a) = toJSON a
