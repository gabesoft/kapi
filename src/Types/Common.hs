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
  , recordCollectionName :: Text
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
-- Representation of an API error
data ApiError = ApiError
  { apiErrorInput :: Maybe Record -- ^ the original input that caused this error
  , apiErrorStatus :: Status -- ^ the HTTP status
  , apiErrorMessage :: LBS.ByteString -- ^ the error message
  } deriving (Eq, Show, Generic)

instance ToJSON ApiError where
  toJSON (ApiError Nothing _ msg) = object ["message" .= LBS.unpack msg]
  toJSON (ApiError (Just r) _ msg) =
    object ["message" .= LBS.unpack msg, "record" .= toJSON r]

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
-- TODO: Replace with ApiItems2
data ApiItem e a
  = Fail e
  | Succ a
  deriving (Eq, Read, Show, Ord)

type ApiResult = ApiItem ApiError Record

toApiItem :: Either e a -> ApiItem e a
toApiItem (Left e) = Fail e
toApiItem (Right a) = Succ a

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

-- TODO: maybe replace ApiItems with this
-- TODO: rename to ApiItem
-- ^
-- Result of a computation on multiple data items. The errors
-- and success results are expected to be lists. In that case
-- @>>=@ collects all errors.
data ApiItems2 e a = ApiItems2
  { fails2 :: e
  , succs2 :: a
  } deriving (Eq, Show, Read, Ord)

concatItems :: [ApiItems2 [e] [a]] -> ApiItems2 [e] [a]
concatItems = foldr (<>) mempty

eitherToItems :: [Either a b] -> ApiItems2 [a] [b]
eitherToItems xs = uncurry ApiItems2 (partitionEithers xs)

itemsToEither :: ApiItems2 [e] [a] -> [Either e a]
itemsToEither (ApiItems2 es as) = (Left <$> es) <> (Right <$> as)

-- TODO: remove after rename ApiItems2 -> ApiItems
itemsToApiResults :: ApiResults2 -> ApiResults
itemsToApiResults (ApiItems2 es as) = ApiItems (Fail <$> es) (Succ <$> as)

type ApiResults2 = ApiItems2 [ApiError] [Record]

instance Eq2 ApiItems2 where
  liftEq2 eq1' eq2' (ApiItems2 e1 a1) (ApiItems2 e2 a2) = eq1' e1 e2 && eq2' a1 a2

instance (Eq e) =>
         Eq1 (ApiItems2 e) where
  liftEq = liftEq2 (==)

instance Bifunctor ApiItems2 where
  bimap f g (ApiItems2 es as) = ApiItems2 (f es) (g as)

instance Functor (ApiItems2 e) where
  fmap f (ApiItems2 es as) = ApiItems2 es (f as)

instance Foldable (ApiItems2 e) where
  foldMap f (ApiItems2 _ as) = f as

instance Traversable (ApiItems2 e) where
  traverse f (ApiItems2 es as) = ApiItems2 es <$> f as

instance (Monoid e, Monoid a) =>
         Monoid (ApiItems2 e a) where
  mempty = ApiItems2 mempty mempty
  mappend (ApiItems2 e1 a1) (ApiItems2 e2 a2) = ApiItems2 (e1 <> e2) (a1 <> a2)

instance Monoid e =>
         Applicative (ApiItems2 e) where
  pure = ApiItems2 mempty
  ApiItems2 _ f <*> ApiItems2 es as = ApiItems2 es (f as)

instance Monoid e =>
         Monad (ApiItems2 e) where
  ApiItems2 es as >>= k =
    let (ApiItems2 e a) = k as
    in ApiItems2 (es <> e) a

instance Monoid e =>
         MonadFix (ApiItems2 e) where
  mfix f =
    let a = f (succs2 a)
    in a

instance ToJSON ApiResults2 where
  toJSON (ApiItems2 [] as) = toJSON as
  toJSON (ApiItems2 [e] []) = toJSON (Fail e :: ApiItem ApiError Record)
  toJSON (ApiItems2 es as) = toJSON ((Succ <$> as) <> (Fail <$> es))

newtype ApiItems2T e m a =
  ApiItems2T (m (ApiItems2 e a))

runApiItems2T :: ApiItems2T e m a -> m (ApiItems2 e a)
runApiItems2T (ApiItems2T m) = m

mapApiItems2T
  :: (n (ApiItems2 e1 a1) -> m (ApiItems2 e2 a2))
  -> ApiItems2T e1 n a1
  -> ApiItems2T e2 m a2
mapApiItems2T f m = ApiItems2T $ f (runApiItems2T m)

-- ^
-- Error counterpart of 'pure'
throwApi
  :: Monoid a
  => e -> ApiItems2 e a
throwApi = flip ApiItems2 mempty

-- ^
-- Error counterpart of 'pure'
throwApiE
  :: (Monad m, Monoid a)
  => e -> ApiItems2T e m a
throwApiE = ApiItems2T . return . throwApi

instance (Eq e, Eq1 m) =>
         Eq1 (ApiItems2T e m) where
  liftEq eq (ApiItems2T a) (ApiItems2T b) = liftEq (liftEq eq) a b

instance (Eq e, Eq1 m, Eq a) =>
         Eq (ApiItems2T e m a) where
  (==) = eq1

instance (Functor m) =>
         Functor (ApiItems2T e m) where
  fmap f = ApiItems2T . fmap (fmap f) . runApiItems2T

instance (Foldable m, Functor m) =>
         Foldable (ApiItems2T e m) where
  foldMap f (ApiItems2T a) = foldMap f (succs2 <$> a)

instance (Traversable m) =>
         Traversable (ApiItems2T e m) where
  traverse f (ApiItems2T a) = ApiItems2T <$> sequenceA (traverse f <$> a)

instance (Functor m, Monad m, Monoid e) =>
         Applicative (ApiItems2T e m) where
  pure = ApiItems2T . return . pure
  ApiItems2T f <*> ApiItems2T a =
    ApiItems2T $ do
      mf <- f
      ma <- a
      return (mf <*> ma)

instance (Monad m, Monoid e) =>
         Monad (ApiItems2T e m) where
  m >>= k =
    ApiItems2T $ do
      (ApiItems2 es as) <- runApiItems2T m
      (ApiItems2 me ma) <- runApiItems2T (k as)
      return (ApiItems2 (es <> me) ma)

instance (Monoid e) =>
         MonadTrans (ApiItems2T e) where
  lift = ApiItems2T . fmap pure

instance (MonadIO m, Monoid e) =>
         MonadIO (ApiItems2T e m) where
  liftIO = lift . liftIO

instance (Monoid e, MonadReader r m) =>
         MonadReader r (ApiItems2T e m) where
  ask = lift ask
  local = mapApiItems2T . local
  reader = lift . reader

instance (Monoid e) =>
         MonadTransControl (ApiItems2T e) where
  type StT (ApiItems2T e) a = ApiItems2 e a
  liftWith f = ApiItems2T $ return <$> f runApiItems2T
  restoreT = ApiItems2T

instance (Monoid e) =>
         MonadBase (ApiItems2 e) (ApiItems2 e) where
  liftBase = id

instance (Monoid e) =>
         MonadBaseControl (ApiItems2 e) (ApiItems2 e) where
  type StM (ApiItems2 e) a = a
  liftBaseWith f = f id
  restoreM = return

instance (Monoid e, MonadBase b m) =>
         MonadBase b (ApiItems2T e m) where
  liftBase = liftBaseDefault

instance (Monoid e, MonadBaseControl b m) =>
         MonadBaseControl b (ApiItems2T e m) where
  type StM (ApiItems2T e m) a = ComposeSt (ApiItems2T e) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

-- ^
-- A collection of 'ApiItem's
-- TODO: maybe we don't need to maintain two separate lists
-- TODO: replace with ApiItems2
data ApiItems e a = ApiItems
  { fails :: [ApiItem e a]
  , succs :: [ApiItem e a]
  } deriving (Eq, Show)

type ApiResults = ApiItems ApiError Record

mkApiItems :: [ApiItem e b] -> [ApiItem e b] -> ApiItems e b
mkApiItems es as = ApiItems (es <> filter isFailure as) (filter isSuccess as)

mkApiItems' :: [ApiItem e b] -> ApiItems e b
mkApiItems' = mkApiItems []

apiItems :: ApiItems e a -> [ApiItem e a]
apiItems (ApiItems es as) = as <> es

mapSucc :: ([ApiItem e b] -> [ApiItem e b]) -> ApiItems e b -> ApiItems e b
mapSucc f (ApiItems es as) = mkApiItems es (f as)

instance Functor (ApiItems e) where
  fmap f (ApiItems es as) = mkApiItems (fmap f <$> es) (fmap f <$> as)

instance Monoid (ApiItems e a) where
  mempty = ApiItems [] []
  mappend (ApiItems e1 a1) (ApiItems e2 a2) = ApiItems (e1 <> e2) (a1 <> a2)

instance Applicative (ApiItems e) where
  pure a = ApiItems [] [Succ a]
  ApiItems _ fs <*> ApiItems es as =
    mkApiItems
      (concatMap (\f -> fmap (f <*>) es) fs)
      (concatMap (\f -> fmap (f <*>) as) fs)

instance Monad (ApiItems e) where
  (ApiItems es as) >>= k =
    let es' = foldr (<>) mempty (f <$> es)
        as' = foldr (<>) mempty (f <$> as)
        f (Fail e) = ApiItems [Fail e] []
        f (Succ a) = k a
    in es' <> as'

instance ToJSON a =>
         ToJSON (ApiItems ApiError a) where
  toJSON (ApiItems es as) = toJSON (as <> es)

-- ^
-- Application name
type AppName = Text

-- ^
-- Api configuration data
data ApiConfig = ApiConfig
  { apiPort :: PortNumber
  , appName :: AppName
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
  cast' (ObjId y) = TermId <$> Just (T.pack $ show y)
  cast' x@(BSON.Bool _) = TermBool <$> cast' x
  cast' x@(BSON.String _) = TermStr <$> cast' x
  cast' x@(BSON.UTC _) = TermDate <$> cast' x
  cast' BSON.Null = Just TermNull
  cast' xs = TermList <$> cast' xs
