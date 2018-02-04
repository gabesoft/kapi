{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^ Common types
module Types.Common where

import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson as AESON
import Data.AesonBson (aesonify, bsonify)
import Data.Bifunctor
import Data.Bson as BSON
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either
import Data.Functor.Classes
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime(..))
import Database.MongoDB (Database, Index(..))
import GHC.Generics
import Network.HTTP.Types.Status
import Network.Socket (HostName, PortNumber)
import Servant
import Text.Read (readMaybe)

-- ^ Api type
type Api = ReaderT ApiConfig Handler

-- ^
-- Representation of a record field definition
data FieldDefinition = FieldDefinition
  { fieldLabel :: Label
  , fieldRequired :: Bool
  , fieldDefault :: Maybe BSON.Value
  , isObjectId :: Bool
  , isJsDate :: Bool
  } deriving (Eq, Show)

-- ^
-- Representation of a record definition
data RecordDefinition = RecordDefinition
  { recordCollection :: Text
  , recordCollectionName :: Text
  , recordIndices :: [Index]
  , recordFields :: Map.Map Label FieldDefinition
  } deriving (Eq, Show)

-- ^
-- Data to be returned from endpoints that return a single or multiple items
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
-- Representation of a record
newtype RecordData a =
  Record [a]
  deriving (Eq, Show)

type RecordId = Text

type Record = RecordData Field

instance Functor RecordData where
  fmap f (Record xs) = Record (f <$> xs)

instance Monoid (RecordData Field) where
  mempty = Record mempty
  mappend (Record xs) (Record ys) = Record (BSON.merge ys xs)

instance ToJSON (RecordData Field) where
  toJSON (Record r) = Object (aesonify r)

instance FromJSON (RecordData Field) where
  parseJSON (Object obj) = return $ Record (bsonify obj)
  parseJSON _ = fail "Could not parse Record"

-- ^
-- Representation of an API error
data ApiError = ApiError
    -- ^ a potential original input that caused this error
  { apiErrorInput :: Maybe Record
    -- ^ the HTTP status (default 500)
  , apiErrorStatus :: Status
    -- ^ the error message
  , apiErrorMessage :: LBS.ByteString
  } deriving (Eq, Show, Generic)

instance ToJSON ApiError where
  toJSON (ApiError Nothing _ msg) = object ["message" .= LBS.unpack msg]
  toJSON (ApiError (Just r) _ msg) =
    object ["message" .= LBS.unpack msg, "input" .= toJSON r]

-- ^
-- The result of a validation operation
newtype ValidationErrors a =
  ValidationErrors [a]
  deriving (Eq, Show)

type ValidationResult = ValidationErrors Field

instance Functor ValidationErrors where
  fmap f (ValidationErrors xs) = ValidationErrors (f <$> xs)

instance Applicative ValidationErrors where
  pure = ValidationErrors . pure
  ValidationErrors fs <*> ValidationErrors es = ValidationErrors (fs <*> es)

instance Monoid (ValidationErrors a) where
  mempty = ValidationErrors mempty
  mappend (ValidationErrors xs) (ValidationErrors ys) =
    ValidationErrors (xs <> ys)

instance ToJSON (RecordData a) =>
         ToJSON (ValidationErrors a) where
  toJSON (ValidationErrors []) = object []
  toJSON (ValidationErrors xs) = toJSON (Record xs)

-- ^
-- Representation of an API item result
-- which could be a failure or a success
data ApiItem e a
  = Fail e
  | Succ a
  deriving (Eq, Read, Show, Ord)

type ApiResult = ApiItem ApiError Record

eitherToApiItem :: Either e a -> ApiItem e a
eitherToApiItem (Left e) = Fail e
eitherToApiItem (Right a) = Succ a

itemToEither :: ApiItem e a -> Either e a
itemToEither (Fail e) = Left e
itemToEither (Succ a) = Right a

isSuccess :: ApiItem a b -> Bool
isSuccess (Fail _) = False
isSuccess (Succ _) = True

isFailure :: ApiItem a b -> Bool
isFailure = not . isSuccess

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

instance ToJSON a =>
         ToJSON (ApiItem ApiError a) where
  toJSON (Fail e) = object ["error" .= toJSON e]
  toJSON (Succ a) = toJSON a

-- ^
-- Result of a computation on multiple data items. The errors
-- and success results are expected to be lists. In that case
-- @>>=@ collects all errors.
data ApiItems e a = ApiItems
  { fails :: e
  , succs :: a
  } deriving (Eq, Show, Read, Ord)

concatItems :: [ApiItems [e] [a]] -> ApiItems [e] [a]
concatItems = foldr (<>) mempty

eitherToItems :: [Either a b] -> ApiItems [a] [b]
eitherToItems xs = uncurry ApiItems (partitionEithers xs)

itemsToEither :: ApiItems [e] [a] -> [Either e a]
itemsToEither (ApiItems es as) = (Right <$> as) <> (Left <$> es)

itemsToResults :: ApiItems [e] [a] -> [ApiItem e a]
itemsToResults items = eitherToApiItem <$> itemsToEither items

resultsToItems :: [ApiItem e a] -> ApiItems [e] [a]
resultsToItems results = eitherToItems $ itemToEither <$> results

throwApi :: Monoid a => e -> ApiItems e a
throwApi = flip ApiItems mempty

type ApiResults = ApiItems [ApiError] [Record]

instance Eq2 ApiItems where
  liftEq2 eq1' eq2' (ApiItems e1 a1) (ApiItems e2 a2) = eq1' e1 e2 && eq2' a1 a2

instance Eq e =>
         Eq1 (ApiItems e) where
  liftEq = liftEq2 (==)

instance Bifunctor ApiItems where
  bimap f g (ApiItems es as) = ApiItems (f es) (g as)

instance Functor (ApiItems e) where
  fmap f (ApiItems es as) = ApiItems es (f as)

instance Foldable (ApiItems e) where
  foldMap f (ApiItems _ as) = f as

instance Traversable (ApiItems e) where
  traverse f (ApiItems es as) = ApiItems es <$> f as

instance (Monoid e, Monoid a) =>
         Monoid (ApiItems e a) where
  mempty = ApiItems mempty mempty
  mappend (ApiItems e1 a1) (ApiItems e2 a2) = ApiItems (e1 <> e2) (a1 <> a2)

instance Monoid e =>
         Applicative (ApiItems e) where
  pure = ApiItems mempty
  ApiItems _ f <*> ApiItems es as = ApiItems es (f as)

instance Monoid e =>
         Monad (ApiItems e) where
  ApiItems es as >>= k =
    let (ApiItems e a) = k as
    in ApiItems (es <> e) a

instance Monoid e =>
         MonadFix (ApiItems e) where
  mfix f =
    let a = f (succs a)
    in a

-- ^
-- Monad transformer variant of 'ApiItems'
newtype ApiItemsT e m a =
  ApiItemsT (m (ApiItems e a))

type ApiResultsT m = ApiItemsT [ApiError] m [Record]

runApiItemsT :: ApiItemsT e m a -> m (ApiItems e a)
runApiItemsT (ApiItemsT m) = m

mapApiItemsT
  :: (n (ApiItems e1 a1) -> m (ApiItems e2 a2))
  -> ApiItemsT e1 n a1
  -> ApiItemsT e2 m a2
mapApiItemsT f m = ApiItemsT $ f (runApiItemsT m)

eitherToItemsT :: Monad m => [Either e a] -> ApiItemsT [e] m [a]
eitherToItemsT = ApiItemsT . return . eitherToItems

throwApiE :: (Monad m, Monoid a) => e -> ApiItemsT e m a
throwApiE = ApiItemsT . return . throwApi

instance (Eq e, Eq1 m) =>
         Eq1 (ApiItemsT e m) where
  liftEq eq (ApiItemsT a) (ApiItemsT b) = liftEq (liftEq eq) a b

instance (Eq e, Eq1 m, Eq a) =>
         Eq (ApiItemsT e m a) where
  (==) = eq1

instance (Functor m) =>
         Functor (ApiItemsT e m) where
  fmap f = ApiItemsT . fmap (fmap f) . runApiItemsT

instance (Foldable m, Functor m) =>
         Foldable (ApiItemsT e m) where
  foldMap f (ApiItemsT a) = foldMap f (succs <$> a)

instance (Traversable m) =>
         Traversable (ApiItemsT e m) where
  traverse f (ApiItemsT a) = ApiItemsT <$> sequenceA (traverse f <$> a)

instance (Functor m, Monad m, Monoid e) =>
         Applicative (ApiItemsT e m) where
  pure = ApiItemsT . return . pure
  ApiItemsT f <*> ApiItemsT a =
    ApiItemsT $ do
      mf <- f
      ma <- a
      return (mf <*> ma)

instance (Monad m, Monoid e) =>
         Monad (ApiItemsT e m) where
  m >>= k =
    ApiItemsT $ do
      (ApiItems es as) <- runApiItemsT m
      (ApiItems me ma) <- runApiItemsT (k as)
      return (ApiItems (es <> me) ma)

instance (Monoid e) =>
         MonadTrans (ApiItemsT e) where
  lift = ApiItemsT . fmap pure

instance (MonadIO m, Monoid e) =>
         MonadIO (ApiItemsT e m) where
  liftIO = lift . liftIO

instance (Monoid e, MonadReader r m) =>
         MonadReader r (ApiItemsT e m) where
  ask = lift ask
  local = mapApiItemsT . local
  reader = lift . reader

instance (Monoid e) =>
         MonadTransControl (ApiItemsT e) where
  type StT (ApiItemsT e) a = ApiItems e a
  liftWith f = ApiItemsT $ return <$> f runApiItemsT
  restoreT = ApiItemsT

instance (Monoid e) =>
         MonadBase (ApiItems e) (ApiItems e) where
  liftBase = id

instance (Monoid e) =>
         MonadBaseControl (ApiItems e) (ApiItems e) where
  type StM (ApiItems e) a = a
  liftBaseWith f = f id
  restoreM = return

instance (Monoid e, MonadBase b m) =>
         MonadBase b (ApiItemsT e m) where
  liftBase = liftBaseDefault

instance (Monoid e, MonadBaseControl b m) =>
         MonadBaseControl b (ApiItemsT e m) where
  type StM (ApiItemsT e m) a = ComposeSt (ApiItemsT e) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

-- ^
-- Api name
type ApiName = Text

-- ^
-- Application configuration data
data AppConfig = AppConfig
  { appPort :: PortNumber
  , appMongoServers :: Map.Map ApiName (HostName, PortNumber)
  , appMongoDbs :: Map.Map ApiName Database
  , appEsIndices :: Map.Map ApiName Database
  , appEsServers :: Map.Map ApiName Text
  } deriving (Eq, Show)

toServer :: (Show a2, Show a1) => (a1, a2) -> String
toServer (host, port) = show host ++ ":" ++ show port

fromServer :: Text -> (HostName, PortNumber)
fromServer server = (T.unpack h, read $ T.unpack p)
  where [h, p] = T.split (== ':') server

instance ToJSON AppConfig where
  toJSON AppConfig {..} =
    object
      [ "port" .= (fromEnum appPort :: Int)
      , "mongo-servers" .= Map.map toServer appMongoServers
      , "mongo-databases" .= appMongoDbs
      , "es-indices" .= appEsIndices
      , "es-servers" .= appEsServers
      ]

instance FromJSON AppConfig where
  parseJSON (Object o) = do
    appPort <- toEnum <$> o .: "port"
    mongoServers <- o .: "mongo-servers"
    appMongoDbs <- o .: "mongo-databases"
    appEsIndices <- o .: "es-indices"
    appEsServers <- o .: "es-servers"

    let appMongoServers = Map.map fromServer mongoServers
    return AppConfig {..}
  parseJSON _ = fail "Expected an object for ApiConfig"

-- ^
-- Api configuration data to be used by a group of endpoints
data ApiConfig = ApiConfig
  { apiPort :: PortNumber
  , mongoServer :: (HostName, PortNumber)
  , mongoDatabase :: Database
  , esIndex :: Database
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
  cast' (ObjId y) = TermId <$> Just (T.pack $ show y)
  cast' x@(BSON.Bool _) = TermBool <$> cast' x
  cast' x@(BSON.String _) = TermStr <$> cast' x
  cast' x@(BSON.UTC _) = TermDate <$> cast' x
  cast' BSON.Null = Just TermNull
  cast' xs = TermList <$> cast' xs
