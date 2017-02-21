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
import Persistence.Facade (validate)
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
  RecordDefinition "feedsubscriptions" "subscriptions" $
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
  RecordDefinition "users" "users" $
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
  RecordDefinition "feeds" "feeds" $
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
  RecordDefinition "posts" "posts" $
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
  RecordDefinition "post" "user-posts" $
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

validateSubscription :: Record -> Either ApiError Record
validateSubscription = validate subscriptionDefinition

validateUserPost :: Record -> Either ApiError Record
validateUserPost = validate userPostDefinition

validateUser :: Record -> Either ApiError Record
validateUser = validate userDefinition

validatePost :: Record -> Either ApiError Record
validatePost = validate postDefinition

validateFeed :: Record -> Either ApiError Record
validateFeed = validate feedDefinition

-- TODO: remove all run methods
-- ^
-- Run a MongoDB action
runDbOld
  :: (MonadBaseControl IO m)
  => (Database -> Pipe -> m (Either Failure a))
  -> Database
  -> Pipe
  -> ExceptT ApiError m a
runDbOld action dbName pipe = do
  results <- lift $ action dbName pipe
  ExceptT (return $ first M.dbToApiError results)

-- ^
-- Run an elastic-search action and extract the results
runEsAndExtractOld
  :: MonadIO m
  => (a -> b -> c -> IO (Either EsError (SearchResult Record)))
  -> a
  -> b
  -> c
  -> ExceptT ApiError m [Record]
runEsAndExtractOld action mappingName server index =
  E.extractRecords [] <$> runEsOld action mappingName server index

-- ^
-- Run an elastic-search action
runEsOld
  :: MonadIO m
  => (mapping -> server -> index -> IO (Either EsError d))
  -> mapping
  -> server
  -> index
  -> ExceptT ApiError m d
runEsOld action mappingName server index = do
  results <- liftIO $ action mappingName server index
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
