{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Common types
module Types.Common where

import Data.Time (UTCTime(..))
import Control.Monad.Reader
import Data.Aeson as AESON
import Data.AesonBson (aesonify, bsonify)
import Data.Bifunctor
import Data.Bson as BSON
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Text (Text)
import Database.MongoDB (Database)
import GHC.Generics
import Network.HTTP.Types.Status
import Network.Socket (HostName, PortNumber)
import Servant

-- | Api type
type Api = ReaderT ApiConfig Handler

-- |
-- Representation of a record schema
data FieldDefinition = FieldDefinition
  { fieldLabel :: Label
  , fieldRequired :: Bool
  , fieldDefault :: Maybe BSON.Value
  } deriving (Eq, Show)

type RecordDefinition = Map.Map Label FieldDefinition

data ApiData a
  = Single a
  | Multiple [a]
  deriving (Eq, Show)

instance ToJSON a =>
         ToJSON (ApiData a) where
  toJSON (Single x) = toJSON x
  toJSON (Multiple xs) = toJSON xs

instance FromJSON a =>
         FromJSON (ApiData a) where
  parseJSON o@(AESON.Object _) = Single <$> parseJSON o
  parseJSON a@(AESON.Array _) = Multiple <$> parseJSON a
  parseJSON _ = fail "Could not parse ApiData"

-- |
-- Representation for a data item
data RecordData a =
  Record [a]
  deriving (Eq, Show)

type RecordId = Text

type Record = RecordData Field

instance ToJSON (RecordData Field) where
  toJSON (Record r) = Object (aesonify r)

instance FromJSON (RecordData Field) where
  parseJSON (Object obj) = return $ Record (bsonify obj)
  parseJSON _ = fail "Could not parse Record"

instance Functor RecordData where
  fmap f (Record xs) = Record (f <$> xs)

instance Monoid (RecordData Field) where
  mempty = Record mempty
  mappend (Record xs) (Record ys) = Record (BSON.merge ys xs)

-- |
-- Representation for an API error
data ApiError = ApiError
  { apiErrorMessage :: LBS.ByteString
  , apiErrorStatus :: Status
  } deriving (Eq, Show, Generic)

instance ToJSON ApiError where
  toJSON err = object ["message" .= LBS.unpack (apiErrorMessage err)]

-- |
-- The result of a record validation
data ValidationResult =
  ValidationErrors [Field]
  deriving (Eq, Show)

instance ToJSON ValidationResult where
  toJSON (ValidationErrors []) = object []
  toJSON (ValidationErrors xs) = toJSON (Record xs)

instance Monoid ValidationResult where
  mempty = ValidationErrors mempty
  mappend (ValidationErrors xs) (ValidationErrors ys) =
    ValidationErrors (xs <> ys)

-- |
-- Representation for an API item result
-- An item result could be an error or a record
data ApiItem e a
  = Fail e
  | Succ a
  deriving (Eq, Show, Ord)

type ApiResult = ApiItem ApiError Record

instance Bifunctor ApiItem where
  bimap f _ (Fail e) = Fail (f e)
  bimap _ g (Succ a) = Succ (g a)

instance Functor (ApiItem e) where
  fmap _ (Fail e) = Fail e
  fmap f (Succ a) = Succ (f a)

instance Applicative (ApiItem e) where
  pure = Succ
  Fail e <*> _ = Fail e
  Succ f <*> a = fmap f a

instance Monad (ApiItem e) where
  Fail e >>= _ = Fail e
  Succ a >>= k = k a

instance (ToJSON a) =>
         ToJSON (ApiItem ApiError a) where
  toJSON (Fail e) = object ["error" .= toJSON e]
  toJSON (Succ a) = toJSON a

-- |
-- Application name
type AppName = Text

-- |
-- Api configuration data
data ApiConfig = ApiConfig
  { apiPort :: PortNumber
  , mongoHost :: HostName
  , mongoPort :: PortNumber
  , mongoDbs :: Map.Map AppName Database
  } deriving (Eq, Show)

-- |
-- Pagination data
data Pagination = Pagination
  { paginationTotal :: Int
  , paginationPage :: Int
  , paginationSize :: Int
  , paginationNext :: Int
  , paginationPrev :: Int
  , paginationFirst :: Int
  , paginationLast :: Int
  , paginationStart :: Int
  , paginationLimit :: Int
  } deriving (Eq, Show, Read)

-- |
-- Filter expressions
data FilterExpr
  = FilterBoolOp FilterBooleanOperator
                 FilterExpr
                 FilterExpr
  | FilterRelOp FilterRelationalOperator
                ColumnName
                FilterTerm
  deriving (Eq, Read, Show)

-- |
-- Relational operators
data FilterRelationalOperator
  = EQ -- equal
  | NQ -- not equal
  | GT -- greater than
  | GE -- greater than or equal
  | LT -- less than
  | LE -- less than or equal
  | IN -- in
  | NN -- not in
  | CONTAINS
  deriving (Eq, Read, Show)

-- |
-- Boolean operators
data FilterBooleanOperator
  = AND
  | OR
  deriving (Eq, Read, Show)

-- |
-- Field or column name
type ColumnName = Text

-- |
-- Filter term
data FilterTerm
  = TermInt Int
  | TermIntList [Int]
  | TermBool Bool
  | TermStr String
  | TermStrList [String]
  | TermDate UTCTime
  | TermDateList [UTCTime]
  | TermNull
  deriving (Eq, Read, Show)
