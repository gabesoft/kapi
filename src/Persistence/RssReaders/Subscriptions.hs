{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- ^
-- Persistence functions for feed subscriptions
module Persistence.RssReaders.Subscriptions
  ( addRead
  , addTags
  , deleteSubscription
  , deleteSubscriptions
  , getSubscription
  , getSubscriptions
  , insertSubscription
  , insertSubscriptions
  , mkFeedIndexedMap
  , mkSubscriptions
  , mkUserPostOnSubCreate
  , mkUserPostOnSubUpdate
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
import Data.Bson
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid ((<>))
import Data.Set ((\\))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Database.V5.Bloodhound as B
import Persistence.Common
import qualified Persistence.ElasticSearch as E
import Persistence.Facade as F
import qualified Persistence.MongoDB as M
import Persistence.RssReaders.Common
import Persistence.RssReaders.UserPosts
       (mkUserPostId, indexUserPosts', mkUserPostsOnCreate,
        mkUserPostOnCreate, mkUserPostOnModify)
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

getSubscription
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => RecordId -> ExceptT ApiError m Record
getSubscription subId = do
  result <- dbGetExisting subscriptionDefinition subId
  counts <- esGetUnreadCounts [ getIdValue' result ]
  return (setSubCount [] counts result)

-- ^
-- Get multiple subscriptions according to the input parameters
getSubscriptions
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => [Field] -> [Field] -> [Field] -> Int -> Int -> ExceptT ApiError m [Record]
getSubscriptions query sort fields start limit = do
  results <- runDb $ M.dbFind subscriptionDefinition query sort fields start limit
  counts <- esGetUnreadCounts (getIdValue' <$> results)
  return (setSubCount fields counts <$> results)

-- ^
-- Populate the unread count field for a subscription
setSubCount :: [Field] -> Map.Map RecordId Int -> Record -> Record
setSubCount fields countMap = set fields
  where
    set [] r = setCount r
    set xs r
      | hasField "unreadCount" (Record xs) = setCount r
      | otherwise = r
    getCount sub = fromMaybe 0 $ Map.lookup (getIdValue' sub) countMap
    setCount sub = setValue "unreadCount" (getCount sub) sub

-- ^
-- Create multiple subscriptions
insertSubscriptions
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [Record] -> ApiItemsT [ApiError] m [Record]
insertSubscriptions input = do
  valid <- validateSubscriptions input
  feeds <- dbGetExistingMulti feedDefinition (feedId <$> valid)
  saved <- dbInsertMulti subscriptionDefinition (mkSubscriptions feeds valid)
  _ <- indexPostsOnSubCreate (filter isEnabled saved)
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
  _ <- runExceptT $ esDeleteUserPostsBySub (filter isDisabled saved)
  _ <- indexPostsOnSubUpdate existing (filter isEnabled saved)
  return saved

-- ^
-- Delete multiple subscriptions by id
deleteSubscriptions
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => [RecordId] -> ApiItemsT [ApiError] m [Record]
deleteSubscriptions ids = do
  deleted <- dbDeleteMulti subscriptionDefinition ids
  _ <- runExceptT $ esDeleteUserPostsBySub deleted
  return deleted

-- ^
-- Index the user-posts associated with all subscriptions being created
indexPostsOnSubCreate
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [Record] -> ApiItemsT [ApiError] m ()
indexPostsOnSubCreate subs = do
  posts <- toMulti (dbGetPostsBySub subs)
  userPosts <- mkRecords posts
  void $ runExceptT (indexUserPosts' userPosts)
  where
    mkRecords posts = mkUserPostsOnCreate subs posts (mkRecord <$> posts)
    mkRecord = mkUserPostOnSubCreate (mkFeedIndexedMap subs)

-- ^
-- Index the user-posts associated with all subscriptions being updated
indexPostsOnSubUpdate
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [Record] -> [Record] -> ApiItemsT [ApiError] m ()
indexPostsOnSubUpdate oldSubs newSubs = do
  posts <- toMulti (dbGetPostsBySub newSubs)
  existing <- toMulti (esGetUserPostsBySub newSubs)
  userPosts <- mkRecords existing posts
  void $ runExceptT (indexUserPosts' userPosts)
  where
    mkRecords existing = runAction (mkRecord $ mkIdIndexedMap existing)
    mkRecord = mkUserPostOnSubUpdate oldSubMap newSubMap
    oldSubMap = mkFeedIndexedMap oldSubs
    newSubMap = mkFeedIndexedMap newSubs

-- ^
-- Make a user-post record to be indexed when updating a subscription
mkUserPostOnSubUpdate
  :: MonadIO m
  => Map.Map RecordId Record
  -> Map.Map RecordId Record
  -> Map.Map RecordId Record
  -> Record
  -> m (Either ApiError (Record, RecordId))
mkUserPostOnSubUpdate oldSubMap newSubMap oldMap post =
  case old of
    Nothing -> mkUserPostOnCreate (Just newSub) (Just post) record
    Just o -> mkUserPostOnModify (Just o) record
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
mkUserPostOnSubCreate :: Map.Map RecordId Record -> Record -> Record
mkUserPostOnSubCreate subMap post = setRead True (mkBaseUserPost sub post)
  where
    sub = fromJust $ Map.lookup (feedId post) subMap

-- ^
-- Make a user-post record populating only the subscription and post id's
mkBaseUserPost :: Record -> Record -> RecordData Field
mkBaseUserPost sub post =
  Record [subIdLabel =: getIdValue' sub, "postId" =: getIdValue' post]

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
addRead :: Record -> Record -> Bool -> Record -> Record
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
esDeleteUserPostsBySub
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => [Record] -> ExceptT ApiError m ()
esDeleteUserPostsBySub [] = return ()
esDeleteUserPostsBySub subs = do
  search <- ExceptT (return $ mkSearchBySub subs)
  void . runEs $ E.deleteByQuery search userPostCollection

-- ^
-- Get the user-posts associated with the given subscriptions
esGetUserPostsBySub
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => [Record] -> ExceptT ApiError m [Record]
esGetUserPostsBySub [] = return []
esGetUserPostsBySub subs = do
  search <- ExceptT . return $ mkSearchBySub subs
  runEsAndExtract (E.searchDocuments search userPostCollection)

-- ^
-- Get the number of unread user-posts for each subscription in
-- the specified subscription ids
esGetUnreadCounts
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => [RecordId] -> ExceptT ApiError m (Map.Map Text Int)
esGetUnreadCounts [] = return Map.empty
esGetUnreadCounts subIds = do
  let search = mkCountsSearch countAggName subIds
  results <- runEs (E.searchDocuments search userPostCollection)
  return (extractCounts countAggName results)

-- ^
-- Create a query that will return the user-posts associated with
-- all specified subscriptions
mkSearchBySub :: [Record] -> Either ApiError B.Search
mkSearchBySub [] = Right E.zeroResultsSearch
mkSearchBySub subs = first E.esToApiError $ E.mkSearchAll (Just filter') [] []
  where
    subIds = getIdValue' <$> subs
    subIdCol = ColumnName subIdLabel 1
    termIds = TermId <$> subIds
    filter' = FilterRelOp In subIdCol (TermList termIds)

-- ^
-- Create a query that will return the number of unread posts per subscription id
mkCountsSearch :: Text -> [RecordId] -> B.Search
mkCountsSearch bucketKey ids =
  B.mkAggregateSearch (Just $ B.QueryBoolQuery boolQuery) termsAgg
  where
    boolQuery = B.mkBoolQuery [unreadQuery, subIdsQuery] [] [] []
    unreadQuery = B.TermQuery (B.Term "read" "false") Nothing
    subIdsQuery = B.TermsQuery subIdLabel (NE.fromList ids)
    termsAgg = B.mkAggregations bucketKey (B.TermsAgg agg)
    agg = (B.mkTermsAggregation subIdLabel) {B.termSize = Just (length ids)}

-- ^
-- Extract the number of unread posts per subscription id from a search result
extractCounts :: Text -> B.SearchResult a -> Map.Map Text Int
extractCounts bucketKey results =
  maybe Map.empty toBucketMap toBuckets
  where
    toBuckets = B.aggregations results >>= B.toTerms bucketKey
    extractVal (B.TextValue v) = v
    extractVal v = error $ "extractCounts: TextValue expected " ++ show v
    extractTerm t = (extractVal (B.termKey t), B.termsDocCount t)
    toBucketMap = Map.fromList . fmap extractTerm . B.buckets

subIdLabel :: Label
subIdLabel = "subscriptionId"

countAggName :: Text
countAggName = "unreadCountsPerSub"

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
