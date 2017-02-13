{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- ^
-- Persistence functions for feed subscriptions
module Persistence.Xandar.Subscriptions where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.Trans.Control
import qualified Data.Aeson as A
import Data.Bifunctor
import Data.Bson hiding (lookup, label)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.Bloodhound (EsError(..), SearchResult(..))
import Database.MongoDB (Database, Pipe, Failure, Index(..))
import Debug.Trace
import Persistence.Common
import qualified Persistence.ElasticSearch as E
import qualified Persistence.MongoDB as M
import Persistence.Xandar.Common
import Persistence.Xandar.UserPosts (insertUserPosts)
import Types.Common

-- ^
-- Insert multiple subscriptions and index related posts
insertSubscriptions :: [Record]
                    -> (Database, Pipe)
                    -> (Text, Text)
                    -> ExceptT ApiError IO [ApiResult]
insertSubscriptions input (dbName, pipe) (server, index) = do
  let validated = validate' <$> input
  let valid = rights validated
  feeds <- runDb (getFeedsById $ feedId <$> valid) dbName pipe
  saved <- mapM save (mkSubscriptions feeds valid)
  posts <- runDb (getPostsBySub $ filter isEnabled saved) dbName pipe
  _ <-
    insertUserPosts
      True
      (mkUserPosts saved posts)
      (dbName, pipe)
      (server, index)
  return $ (Succ <$> saved) <> (Fail <$> lefts validated)
  where
    save r = saveDb M.dbInsert r dbName pipe

-- ^
-- Update multiple subscriptions and index related posts
updateSubscriptions replace input (dbName, pipe) (server, index) = do
  let validated = validateHasId' <$> input
  let valid = rights validated
  existing <- runDb (getSubsById $ getIdValue' <$> valid) dbName pipe
  let existingMap = mkRecordMap existing
  let subPairs = mkPair existingMap <$> valid
  let subs = rights $ snd <$> subPairs
  let errs = lefts $ snd <$> subPairs
  saved <- mapM save subs

  -- TODO: index posts for enabled (read = enabled changed)
  --       and update tags
  --       delete posts for disabled

  return $ (Succ <$> saved) <> (Fail <$> lefts validated <> errs)
  where
    save r = saveDb M.dbUpdate r dbName pipe
    mkPair existingMap r = (r, mkRecord (get existingMap r) r)
    get m r = Map.lookup (getIdValue' r) m
    mkRecord Nothing r = Left mkApiError404
    mkRecord (Just e) r = Right $ mergeRecords' replace e r

mkUserPosts subs posts = mkUserPost <$> posts
  where
    subMap = mkRecordMap' "feedId" subs
    getSub post = fromJust $ Map.lookup (feedId post) subMap
    mkUserPost post =
      Record
        [ "subscriptionId" =: getIdValue' (getSub post)
        , "postId" =: getIdValue' post
        , "read" =: True
        ]

mkSubscriptions :: [Record] -> [Record] -> [Record]
mkSubscriptions feeds = fmap mkSub
  where
    feedMap = mkRecordMap feeds
    mkSub r = setValue "title" (getTitle r $ getFeed r) r
    getFeed r = Map.lookup (feedId r) feedMap
    getTitle :: Record -> Maybe Record -> Text
    getTitle r f =
      fromJust $
      getValue "title" r <|> (f >>= getValue "title") <|> Just "Untitled"

-- ^
-- Get all posts related to all input subscriptions. The records
-- returned will only contain the id and the feedId fields.
getPostsBySub
  :: (MonadBaseControl IO m, MonadIO m)
  => [Record] -> Database -> Pipe -> m (Either Failure [Record])
getPostsBySub subs = M.dbFind postDefinition query [] fields 0 0
  where
    query = M.idsQuery' "feedId" (feedId <$> subs)
    fields = M.mkIncludeFields ["feedId"]

-- ^
-- Get multiple subscriptions by id
getSubsById
  :: (MonadBaseControl IO m, MonadIO m)
  => [RecordId] -> Database -> Pipe -> m (Either Failure [Record])
getSubsById = getRecordsById subscriptionDefinition

-- ^
-- Get multiple feeds by id
getFeedsById
  :: (MonadBaseControl IO m, MonadIO m)
  => [RecordId] -> Database -> Pipe -> m (Either Failure [Record])
getFeedsById = getRecordsById subscriptionDefinition

-- ^
-- Insert or update a valid record
saveDb
  :: (MonadBaseControl IO m, MonadIO m)
  => (RecordDefinition -> Record -> Database -> Pipe -> m (Either Failure RecordId))
  -> Record
  -> Database
  -> Pipe
  -> ExceptT ApiError m Record
saveDb action input dbName pipe = do
  let def = subscriptionDefinition
  uid <- runDb (action def $ populateDefaults def input) dbName pipe
  record <- runDb (M.dbGetById def uid) dbName pipe
  return (fromJust record)

-- ^
-- Get the feed id of a subscription
feedId :: Record -> RecordId
feedId = getValue' "feedId"

-- ^
-- Return true if a subscription is enabled
isEnabled = not . isValueOn "disabled"

-- ^
-- Validate a subscription
validate' :: Record -> Either ApiError Record
validate' = validateRecord subscriptionDefinition