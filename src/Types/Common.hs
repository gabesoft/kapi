{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^ Common types
module Types.Common where

import Control.Monad.Reader
import Data.Aeson as AESON
import Data.AesonBson (aesonify, bsonify)
import Data.Bifunctor
import Data.Bson as BSON
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime(..))
import Database.MongoDB (Database)
import GHC.Generics
import Network.HTTP.Types.Status
import Network.Socket (HostName, PortNumber)
import Servant
import Text.Read (readMaybe)

-- ^ Api type
type Api = ReaderT ApiConfig Handler

-- ^
-- Representation of a record schema
data FieldDefinition = FieldDefinition
  { fieldLabel :: Label
  , fieldRequired :: Bool
  , fieldDefault :: Maybe BSON.Value
  , isObjectId :: Bool
  } deriving (Eq, Show)

data RecordDefinition = RecordDefinition
  { recordCollection :: Text
  , recordFields :: Map.Map Label FieldDefinition
  } deriving (Eq, Show)

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

-- ^
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

-- ^
-- Representation for an API error
data ApiError = ApiError
  { apiErrorStatus :: Status
  , apiErrorMessage :: LBS.ByteString
  } deriving (Eq, Show, Generic)

instance ToJSON ApiError where
  toJSON err = object ["message" .= LBS.unpack (apiErrorMessage err)]

-- ^
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

-- ^
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

-- ^
-- Application name
type AppName = Text

-- ^
-- Api configuration data
data ApiConfig = ApiConfig
  { apiPort :: PortNumber
  , mongoHost :: HostName
  , mongoPort :: PortNumber
  , mongoDbs :: Map.Map AppName Database
  , esIndices :: Map.Map AppName Database
  , esServer :: Text
  } deriving (Eq, Show)

-- ^
-- Pagination data
data Pagination = Pagination
  { paginationTotal :: Int
  , paginationPage :: PageNumber
  , paginationSize :: PageSize
  , paginationNext :: Int
  , paginationPrev :: Int
  , paginationFirst :: Int
  , paginationLast :: Int
  , paginationStart :: RecordStart
  , paginationLimit :: ResultLimit
  } deriving (Eq, Show, Read)

type PageNumber = Int

type PageSize = Int

type RecordStart = Int

type ResultLimit = Int

-- ^
-- Sort expression
data SortExpr =
  SortExpr Label
           SortDirection
  deriving (Eq, Read, Show)

-- ^
-- Sort direction
data SortDirection
  = SortAscending
  | SortDescending
  deriving (Eq, Read, Show)

-- ^
-- Filter expressions
data FilterExpr
  = FilterBoolOp FilterBooleanOperator
                 FilterExpr
                 FilterExpr
  | FilterRelOp FilterRelationalOperator
                ColumnName
                FilterTerm
  deriving (Eq, Read, Show)

-- ^
-- Relational operators
data FilterRelationalOperator
  = Equal
  | NotEqual
  | GreaterThan
  | GreaterThanOrEqual
  | LessThan
  | LessThanOrEqual
  | In
  | NotIn
  | Contains
  | NotContains
  deriving (Eq, Read, Show)

-- ^
-- Boolean operators
data FilterBooleanOperator
  = And
  | Or
  deriving (Eq, Read, Show)

-- ^
-- Field or column name
data ColumnName =
  ColumnName Text
             ColumnBoost
  deriving (Eq, Read, Show)

type ColumnBoost = Double

instance IsString ColumnName where
  fromString name = ColumnName (T.pack name) 1

-- ^
-- Filter term
data FilterTerm
  = TermInt Int
  | TermFloat Double
  | TermBool Bool
  | TermStr Text
  | TermId Text
  | TermDate UTCTime
  | TermNull
  | TermList [FilterTerm]
  deriving (Eq, Read, Show)

instance Val FilterTerm where
  val (TermInt x) = val x
  val (TermFloat x) = val x
  val (TermBool x) = val x
  val (TermStr x) = val x
  val (TermId x) = val (ObjId (fromJust . readMaybe $ T.unpack x))
  val (TermDate x) = val x
  val TermNull = BSON.Null
  val (TermList xs) = valList xs
  cast' x@(Int32 _) = TermInt <$> cast' x
  cast' x@(Int64 _) = TermInt <$> cast' x
  cast' x@(Float _) = TermFloat <$> cast' x
  cast' x@(ObjId y) = TermId <$> Just (T.pack $ show y)
  cast' x@(BSON.Bool _) = TermBool <$> cast' x
  cast' x@(BSON.String _) = TermStr <$> cast' x
  cast' x@(BSON.UTC _) = TermDate <$> cast' x
  cast' BSON.Null = Just TermNull
  cast' xs = TermList <$> cast' xs
