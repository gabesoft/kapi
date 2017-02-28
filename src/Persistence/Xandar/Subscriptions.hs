{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- ^
-- Persistence functions for feed subscriptions
module Persistence.Xandar.Subscriptions
  ( addRead
  , addTags
  , deleteSubscription
  , deleteSubscriptions
  , insertSubscription
  , insertSubscriptions
  , mkSubscriptions
  , modifySubscription
  , modifySubscriptions
  , replaceSubscription
  , replaceSubscriptions
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Bifunctor
import Data.Bson hiding (lookup, label)
import Data.Either
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid ((<>))
import Data.Set ((\\))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Database.Bloodhound as B
import Database.MongoDB (Database, Pipe, Failure)
import Debug.Trace
import Persistence.Common
import qualified Persistence.ElasticSearch as E
import Persistence.Facade as F
import qualified Persistence.MongoDB as M
import Persistence.Xandar.Common
import Persistence.Xandar.UserPosts
       (mkUserPostId, mkUserPostId', indexUserPosts', createUserPosts,
        createUserPost, modifyUserPost)
import qualified Persistence.Xandar.UserPosts as U
import Types.Common

insertSubscription
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => Record -> ExceptT ApiError m Record
insertSubscription = toSingle insertSubscriptions

replaceSubscription
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => Record -> ExceptT ApiError m Record
replaceSubscription = toSingle replaceSubscriptions

modifySubscription
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => Record -> ExceptT ApiError m Record
modifySubscription = toSingle modifySubscriptions

deleteSubscription
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => RecordId -> ExceptT ApiError m Record
deleteSubscription = toSingle deleteSubscriptions

modifySubscriptions
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => [Record] -> ApiItemsT [ApiError] m [Record]
modifySubscriptions = updateSubscriptions False

replaceSubscriptions
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => [Record] -> ApiItemsT [ApiError] m [Record]
replaceSubscriptions = updateSubscriptions True

-- ^
-- Create multiple subscriptions
insertSubscriptions
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [Record] -> ApiItemsT [ApiError] m [Record]
insertSubscriptions input = do
  valid <- validateSubscriptions input
  feeds <- dbGetExistingMulti feedDefinition (feedId <$> valid)
  saved <- dbInsertMulti subscriptionDefinition (mkSubscriptions feeds valid)
  _ <- indexPostsOnCreate (filter isEnabled saved)
  return saved

-- ^
-- Update multiple subscriptions
updateSubscriptions
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => Bool -> [Record] -> ApiItemsT [ApiError] m [Record]
updateSubscriptions replace input = do
  valid1 <- validateDbIdMulti input
  existing <- dbGetExistingMulti subscriptionDefinition (getIdValue' <$> valid1)
  let merged = mergeFromMap replace (mkIdIndexedMap valid1) <$> existing
  valid2 <- validateSubscriptions merged
  saved <- dbUpdateMulti replace subscriptionDefinition valid2
  _ <- runExceptT $ esDeleteUserPosts (filter isDisabled saved)
  _ <- indexPostsOnUpdate existing (filter isEnabled saved)
  return saved

-- ^
-- Delete multiple subscriptions by id
deleteSubscriptions
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => [RecordId] -> ApiItemsT [ApiError] m [Record]
deleteSubscriptions ids = do
  deleted <- dbDeleteMulti subscriptionDefinition ids
  _ <- runExceptT $ esDeleteUserPosts deleted
  return deleted

-- ^
-- Index the user-posts associated with all subscriptions being created
indexPostsOnCreate
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [Record] -> ApiItemsT [ApiError] m ()
indexPostsOnCreate subs = do
  posts <- toMulti (dbGetPostsBySub subs)
  userPosts <- mkRecords posts
  void $ runExceptT (indexUserPosts' userPosts)
  where
    mkRecords posts = createUserPosts subs posts (mkRecord <$> posts)
    mkRecord = mkUserPostOnCreate (mkFeedIndexedMap subs)

-- ^
-- Index the user-posts associated with all subscriptions being updated
indexPostsOnUpdate
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [Record] -> [Record] -> ApiItemsT [ApiError] m ()
indexPostsOnUpdate oldSubs newSubs = do
  posts <- toMulti (dbGetPostsBySub newSubs)
  existing <- toMulti (esGetUserPostsBySubs newSubs)
  userPosts <- mkRecords existing posts
  void $ runExceptT (indexUserPosts' userPosts)
  where
    mkRecords existing = runAction (mkRecord $ mkIdIndexedMap existing)
    mkRecord = mkUserPostOnUpdate oldSubMap newSubMap
    oldSubMap = mkFeedIndexedMap oldSubs
    newSubMap = mkFeedIndexedMap newSubs

-- ^
-- Make a user-post record to be indexed when updating a subscription
mkUserPostOnUpdate
  :: MonadIO m
  => Map.Map RecordId Record
  -> Map.Map RecordId Record
  -> Map.Map RecordId Record
  -> Record
  -> m (Either ApiError (Record, RecordId))
mkUserPostOnUpdate oldSubMap newSubMap oldMap post =
  case old of
    Nothing -> createUserPost (Just newSub) (Just post) record
    Just o -> modifyUserPost (Just o) record
  where
    getSub = fromJust . Map.lookup (feedId post)
    oldSub = getSub oldSubMap
    newSub = getSub newSubMap
    rId = mkUserPostId (getIdValue' newSub) (getIdValue' post)
    old = Map.lookup rId oldMap
    record =
      addTags oldSub newSub old $
      addRead oldSub newSub (isNothing old) $ mkBaseUserPost newSub post

-- ^
-- Make a user-post record to be indexed when creating a subscription
mkUserPostOnCreate :: Map.Map RecordId Record -> Record -> Record
mkUserPostOnCreate subMap post = setRead True (mkBaseUserPost sub post)
  where
    sub = fromJust $ Map.lookup (feedId post) subMap

-- ^
-- Make a user-post record populating only the subscription and post id's
mkBaseUserPost :: Record -> Record -> RecordData Field
mkBaseUserPost sub post =
  Record ["subscriptionId" =: getIdValue' sub, "postId" =: getIdValue' post]

-- ^
-- Populate the tags field of a user-post based on the delta
-- between an old and a new subscription
addTags :: Record -> Record -> Maybe Record -> Record -> Record
addTags oldSub newSub existing = setTags compute
  where
    tags = Set.fromList $ maybe [] getTags existing
    getTags :: Record -> [Text]
    getTags = fromMaybe [] . getValue "tags"
    oldTags = Set.fromList (getTags oldSub)
    newTags = Set.fromList (getTags newSub)
    compute = Set.toList $ (tags \\ oldTags) <> newTags

-- ^
-- Populate the read field of a user-post based on whether
-- a subscription has just been enabled or this is a new record
addRead oldSub newSub isNew record
  | (old /= new) || isNew = setRead True record
  | otherwise = record
  where
    old = isEnabled oldSub
    new = isEnabled newSub

-- ^
-- Create subscription records for all given feeds
mkSubscriptions :: [Record] -> [Record] -> [Record]
mkSubscriptions feeds = fmap mkSub
  where
    feedMap = mkIdIndexedMap feeds
    mkSub r = setValue "title" (getTitle r $ getFeed r) r
    getFeed r = Map.lookup (feedId r) feedMap
    getTitle :: Record -> Maybe Record -> Text
    getTitle r f =
      fromMaybe "Untitled" $ getValue "title" r <|> (f >>= getValue "title")

-- ^
-- Get the posts related to all input subscriptions. The records
-- returned will only contain the id and the feedId fields.
dbGetPostsBySub
  :: (MonadReader ApiConfig m, MonadBaseControl IO m, MonadIO m)
  => [Record] -> ExceptT ApiError m [Record]
dbGetPostsBySub [] = return []
dbGetPostsBySub subs = runDb $ M.dbFind postDefinition query [] [] 0 0
  where
    query = M.idsQuery' "feedId" (feedId <$> subs)

-- ^
-- Remove all user-posts associated with the given subscriptions
-- from the search index
esDeleteUserPosts
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => [Record] -> ExceptT ApiError m ()
esDeleteUserPosts [] = return ()
esDeleteUserPosts subs = do
  search <- ExceptT (return $ mkSearchBySubs subs)
  void . runEs $ E.deleteByQuery search userPostCollection

esGetUserPostsBySubs
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => [Record] -> ExceptT ApiError m [Record]
esGetUserPostsBySubs [] = return []
esGetUserPostsBySubs subs = do
  search <- ExceptT . return $ mkSearchBySubs subs
  runEsAndExtract (E.searchDocuments search userPostCollection)

-- ^
-- Create a query that would return the user-posts associated with
-- all specified subscriptions
mkSearchBySubs :: [Record] -> Either ApiError B.Search
mkSearchBySubs [] = Right E.zeroResultsSearch
mkSearchBySubs subs = first E.esToApiError $ E.mkSearchAll (Just filter') [] []
  where
    subIds = getIdValue' <$> subs
    filter' = FilterRelOp In "subscriptionId" (TermList $ TermId <$> subIds)

-- ^
-- Make a record map indexed by the values of the feedId field
mkFeedIndexedMap :: [Record] -> Map.Map RecordId Record
mkFeedIndexedMap = mkRecordMap "feedId"

-- ^
-- Get the feed id of a subscription
feedId :: Record -> RecordId
feedId = getValue' "feedId"

-- ^
-- Return true if a subscription is disabled
isDisabled :: Record -> Bool
isDisabled = isValueOn "disabled"

-- ^
-- Return true if a subscription is enabled
isEnabled :: Record -> Bool
isEnabled = not . isDisabled

-- ^
-- Set the value of the read field
setRead :: Bool -> Record -> Record
setRead = setValue "read"

-- ^
-- Set the value of the tags field
setTags :: [Text] -> Record -> Record
setTags = setValue "tags"
