{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Common functionality for persistence modules
module Persistence.Xandar.Common where

import Control.Monad.Except
import Control.Monad.Trans.Control
import qualified Data.Aeson as A
import Data.Bifunctor
import Data.Bson hiding (lookup, label)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid ((<>))
import Database.Bloodhound (EsError(..), SearchResult(..))
import Database.MongoDB (Database, Pipe, Failure, Index(..))
import Persistence.Common
import qualified Persistence.ElasticSearch as E
import qualified Persistence.MongoDB as M
import Types.Common

subscriptionIndices :: [Index]
subscriptionIndices =
  [ Index
    { iColl = recordCollection subscriptionDefinition
    , iKey = ["userId" =: (1 :: Int), "feedId" =: (1 :: Int)]
    , iName = "userid_feedid_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  ]

subscriptionDefinition :: RecordDefinition
subscriptionDefinition =
  RecordDefinition "feedsubscriptions" $
  Map.fromList
    [ mkIdDef "userId"
    , mkIdDef "feedId"
    , mkOptDef' "title"
    , mkOptDef' "notes"
    , mkOptDef "tags" (Just [] :: Maybe [String])
    , mkReqDef "disabled" (Just False :: Maybe Bool)
    , mkOptDef "unreadCount" (Just 0 :: Maybe Int)
    ]

userIndices :: [Index]
userIndices =
  [ Index
    { iColl = recordCollection userDefinition
    , iKey = ["email" =: (1 :: Int)]
    , iName = "email_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  ]

userDefinition :: RecordDefinition
userDefinition =
  RecordDefinition "users" $
  Map.fromList
    [ mkReqDef' "email"
    , mkReqDef "disabled" (Just False)
    , mkReqDef "admin" (Just False)
    , mkOptDef' "githubAvatar"
    , mkOptDef' "githubUrl"
    , mkOptDef' "githubLogin"
    ]

feedIndices :: [Index]
feedIndices =
  [ Index
    { iColl = recordCollection feedDefinition
    , iKey = ["uri" =: (1 :: Int)]
    , iName = "uri_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  ]

feedDefinition :: RecordDefinition
feedDefinition =
  RecordDefinition "feeds" $
  Map.fromList
    [ mkOptDef' "author"
    , mkOptDef' "data"
    , mkOptDef' "date"
    , mkOptDef' "description"
    , mkReqDef "failedAttempts" (Just 0 :: Maybe Int)
    , mkOptDef' "favicon"
    , mkOptDef' "format"
    , mkOptDef' "generator"
    , mkOptDef' "guid"
    , mkOptDef' "image"
    , mkOptDef' "language"
    , mkOptDef' "lastPostDate"
    , mkOptDef' "lastReadDate"
    , mkOptDef' "lastReadStatus"
    , mkOptDef' "link"
    , mkReqDef "postCount" (Just 0 :: Maybe Int)
    , mkOptDef' "title"
    , mkReqDef' "uri"
    ]

postIndices :: [Index]
postIndices =
  [ Index
    { iColl = recordCollection postDefinition
    , iKey = ["guid" =: (1 :: Int)]
    , iName = "guid_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  , Index
    { iColl = recordCollection postDefinition
    , iKey = ["link" =: (1 :: Int)]
    , iName = "link_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  ]

postDefinition :: RecordDefinition
postDefinition =
  RecordDefinition "posts" $
  Map.fromList
    [ mkOptDef' "author"
    , mkOptDef' "comments"
    , mkOptDef' "date"
    , mkOptDef' "description"
    , mkIdDef "feedId"
    , mkReqDef' "guid"
    , mkOptDef' "image"
    , mkOptDef' "inlineStatus"
    , mkReqDef' "link"
    , mkOptDef' "pubdate"
    , mkOptDef' "source"
    , mkOptDef' "summary"
    , mkOptDef' "title"
    ]

userPostDefinition :: RecordDefinition
userPostDefinition =
  RecordDefinition "post" $
  Map.fromList
    [ mkReqDef' "subscriptionId"
    , mkReqDef' "postId"
    , mkOptDef' "feedId"
    , mkOptDef "read" (Just False)
    , mkOptDef' "userId"
    , mkOptDef' "post"
    , mkOptDef' "title"
    , mkOptDef' "tags"
    ]

-- ^
-- Validate a record against its definition
validateRecord :: RecordDefinition -> Record -> Either ApiError Record
validateRecord def record = vResultToEither (validate def record)

-- ^
-- Validate that a record contains a valid id field
validateHasId' :: Record -> Either ApiError Record
validateHasId' = vResultToEither . M.validateHasId

-- ^
-- Run a MongoDB action
runDb
  :: (MonadBaseControl IO m)
  => (Database -> Pipe -> m (Either Failure c))
  -> Database
  -> Pipe
  -> ExceptT ApiError m c
runDb action dbName pipe = do
  results <- lift $ action dbName pipe
  ExceptT (return $ first M.dbToApiError results)

-- ^
-- Run an elastic-search action and extract the results
runEsAndExtract
  :: MonadIO m
  => (a -> b -> c -> IO (Either EsError (SearchResult Record)))
  -> a
  -> b
  -> c
  -> ExceptT ApiError m [Record]
runEsAndExtract action server index mappingName = do
  results <- runEs action server index mappingName
  return $ E.extractRecords [] results

-- ^
-- Run an elastic-search action
runEs
  :: MonadIO m
  => (a -> b -> c -> IO (Either EsError d))
  -> a
  -> b
  -> c
  -> ExceptT ApiError m d
runEs action server index mappingName = do
  results <- liftIO $ action server index mappingName
  ExceptT (return $ first E.esToApiError results)

-- ^
-- Get multiple records by id
getRecordsById
  :: (MonadBaseControl IO m, MonadIO m)
  => RecordDefinition
  -> [RecordId]
  -> Database
  -> Pipe
  -> m (Either Failure [Record])
getRecordsById def ids = M.dbFind def (M.idsQuery ids) [] [] 0 0

-- ^
-- Make a make a map with the ids as keys and records as values
mkIdIndexedMap :: [Record] -> Map.Map RecordId Record
mkIdIndexedMap = mkRecordMap idLabel

-- ^
-- Merge an existing and an updated record according to the 'replace' flag
mergeRecords' :: Bool -> Record -> Record -> Record
mergeRecords' True = replaceRecords [createdAtLabel, updatedAtLabel, idLabel]
mergeRecords' False = mergeRecords

-- ^
-- Make a make a map keyed by the specified field and having records as values
mkRecordMap :: Label -> [Record] -> Map.Map RecordId Record
mkRecordMap label xs = Map.fromList (addId <$> xs)
  where
    addId r = (getValue' label r, r)

lookup'
  :: Eq a
  => a -> [(a, c)] -> c
lookup' name = fromJust . lookup name
