{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- ^
-- Persistence functions for feed subscriptions
module Persistence.Xandar.Subscriptions where

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
       (insertUserPosts, mkUserPostId, mkUserPostId', indexUserPosts,
        indexUserPosts', indexDocumentsOld, createUserPosts,
        createUserPost, modifyUserPost)
import qualified Persistence.Xandar.UserPosts as U
import Types.Common

-- TODO: rename to insertSubscriptions after removing others
dbInsertSubscriptions
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [Record] -> ApiItems2T [ApiError] m [Record]
dbInsertSubscriptions input = do
  valid <- validateMulti subscriptionDefinition input
  feeds <- getExistingMulti feedDefinition (feedId <$> valid)
  saved <- dbInsertMulti subscriptionDefinition (mkSubscriptions feeds valid)
  _ <- indexPostsOnCreate (filter isEnabled saved)
  return saved

dbUpdateSubscriptions replace input = do
  valid1 <- validateDbIdMulti input
  existing <- getExistingMulti subscriptionDefinition (getIdValue' <$> valid1)
  let merged = mergeFromMap replace (mkIdIndexedMap valid1) <$> existing
  valid2 <- validateMulti subscriptionDefinition merged
  saved <- dbInsertMulti subscriptionDefinition valid2
  _ <- runExceptT $ deletePosts (filter isDisabled saved)
  _ <- indexPostsOnUpdate existing (filter isEnabled saved)
  return saved

indexPostsOnCreate
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [Record] -> ApiItems2T [ApiError] m ()
indexPostsOnCreate subs = do
  posts <- toMulti (getPostsBySub subs)
  userPosts <-
    createUserPosts
      (subs, posts)
      (mkUserPostOnCreate (mkFeedIndexedMap subs) <$> posts)
  void $ runExceptT (indexUserPosts' userPosts)

indexPostsOnUpdate oldSubs newSubs = do
  posts <- toMulti (getPostsBySub newSubs)
  existing <- toMulti (getUserPostsBySubs newSubs)
  userPosts <- mkUserPostsOnUpdate oldSubs newSubs existing posts
  void $ runExceptT (indexUserPosts' userPosts)

-- TODO: consolidate with updateUserPosts & createUserPosts
mkUserPostsOnUpdate oldSubs newSubs existing posts = do
  records <- mapM (mkUserPostOnUpdate oldSubMap newSubMap existingMap) posts
  ApiItems2T . return $ eitherToItems records
  where
    oldSubMap = mkFeedIndexedMap oldSubs
    newSubMap = mkFeedIndexedMap newSubs
    existingMap = mkIdIndexedMap existing

mkUserPostOnUpdate oldSubMap newSubMap oldMap post =
  case old of
    Nothing -> createUserPost (Just newSub, Just post) record
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

addTags :: Record -> Record -> Maybe Record -> Record -> Record
addTags oldSub newSub existing = setTags compute
  where
    tags = Set.fromList $ maybe [] getTags existing
    getTags :: Record -> [Text]
    getTags = fromMaybe [] . getValue "tags"
    setTags = setValue "tags"
    oldTags = Set.fromList (getTags oldSub)
    newTags = Set.fromList (getTags newSub)
    compute = Set.toList $ (tags \\ oldTags) <> newTags

-- ^
-- Populate the read field based on whether the new subscription
-- has just been enabled or this is a new user-post
addRead oldSub newSub isNew record
  | old /= new = setRead True record
  | isNew = setRead True record
  | otherwise = record
  where old = isEnabled oldSub
        new = isEnabled newSub

setRead :: Val a => a -> Record -> Record
setRead = setValue "read"

mkUserPostOnCreate :: Map.Map RecordId Record -> Record -> Record
mkUserPostOnCreate subMap post = setRead True (mkBaseUserPost sub post)
  where
    sub = fromJust $ Map.lookup (feedId post) subMap

mkBaseUserPost :: Record -> Record -> RecordData Field
mkBaseUserPost sub post =
  Record ["subscriptionId" =: getIdValue' sub, "postId" =: getIdValue' post]

-- ^
-- Update multiple subscriptions and index related posts
updateSubscriptions
  :: Bool
  -> [Record]
  -> (Database, Pipe)
  -> (Text, Text)
  -> ExceptT ApiError IO ApiResults
updateSubscriptions replace input (dbName, pipe) (server, index) = do
  let validated1 = validateId <$> input
  let valid = rights validated1
  existing <- runDbOld (getSubsById $ getIdValue' <$> valid) dbName pipe
  let records = mkRecord (mkIdIndexedMap existing) <$> valid
  let validated2 = validateSubscription <$> rights records
  saved <- mapM save (rights validated2)
  _ <- insertUserPosts' existing saved (dbName, pipe) (server, index)
  let disabledIds = getIdValue' <$> filter (not . isEnabled) saved
  deletePostsOld disabledIds server index userPostCollection
  return $
    mkApiItems
      (Fail <$> lefts validated1 <> lefts records <> lefts validated2)
      (Succ <$> saved)
  where
    save r = saveDb M.dbUpdate r dbName pipe
    get m r = Map.lookup (getIdValue' r) m
    mkRecord existingMap r = mkRecord' (get existingMap r) r
    mkRecord' Nothing _ = Left mkApiError404
    mkRecord' (Just e) r = Right $ mergeRecords' replace e r

-- ^
-- Insert multiple subscriptions and index related posts
insertSubscriptions :: [Record]
                    -> (Database, Pipe)
                    -> (Text, Text)
                    -> ExceptT ApiError IO ApiResults
insertSubscriptions input (dbName, pipe) (server, index) = do
  let validated = validateSubscription <$> input
  let valid = rights validated
  feeds <- runDbOld (getFeedsById $ feedId <$> valid) dbName pipe
  saved <- mapM save (mkSubscriptions feeds valid)
  posts <- runDbOld (getPostsBySubOld $ filter isEnabled saved) dbName pipe
  _ <-
    insertUserPosts -- TODO: use custom index method since we have all data necessary
      True
      (mkUserPosts saved posts)
      (dbName, pipe)
      (server, index)
  return $ mkApiItems (Fail <$> lefts validated) (Succ <$> saved)
  where
    save r = saveDb M.dbInsert r dbName pipe

insertUserPosts'
  :: [Record]
  -> [Record]
  -> (Database, Pipe)
  -> (Text, Text)
  -> ExceptT ApiError IO [ApiItem ApiError Record]
insertUserPosts' eSubs nSubs (dbName, pipe) (server, index) = do
  let mapping = recordCollection userPostDefinition
  posts <- runDbOld (getPostsBySubOld $ filter isEnabled nSubs) dbName pipe
  let userPostIds = getUserPostIds (mkFeedIndexedMap nSubs) posts
  existing <- runEsAndExtractOld (E.getByIds userPostIds) mapping server index
  results <- mkUserPosts'' (eSubs, nSubs, posts) existing
  created <- indexDocumentsOld (rights results) mapping server index
  return $ (Succ <$> created) <> (Fail <$> lefts results)

-- TODO: handle other cases
addTagsOld :: Record -> Maybe Record -> Maybe Record -> Record -> Record
addTagsOld _ Nothing _ r = r
addTagsOld _ _ Nothing r = r
addTagsOld nSub (Just eSub) (Just existing) r = setTags (computeTagsOld eTags nTags $ getTags existing) r
  where
    getTags :: Record -> [Text]
    getTags = fromMaybe [] . getValue "tags"
    setTags = setValue "tags"
    eTags = getTags eSub
    nTags = getTags nSub

computeTagsOld :: Ord a => [a] -> [a] -> [a] -> [a]
computeTagsOld eTags nTags tags = Set.toList $ (tSet \\ eSet) <> nSet
  where eSet = Set.fromList eTags
        nSet = Set.fromList nTags
        tSet = Set.fromList tags

mkUserPosts''
  :: (MonadIO m)
  => ([Record], [Record], [Record])
  -> [Record]
  -> m [Either ApiError (Record, RecordId)]
mkUserPosts'' (eSubs, nSubs, posts) existing = do
  results <- U.mkUserPosts True (nSubs, posts) (existing, records)
  return $ fmap setTags <$> results
  where
    eSubMap = mkFeedIndexedMap eSubs
    nSubMap = mkFeedIndexedMap nSubs
    existingMap = mkIdIndexedMap existing
    records = mkUserPosts' eSubs nSubs posts
    setTags (r, rid) =
      flip (,) rid $
      addTagsOld (getSub' r nSubMap) (getSub r eSubMap) (getExisting rid) r
    getSub' r = fromJust . getSub r
    getSub r = Map.lookup (getValue' "feedId" r)
    getExisting rid = Map.lookup rid existingMap

-- ^
-- Make user posts for existing subscriptions
-- TODO: consolidate the two mkUserPosts functions
mkUserPosts'
  :: Functor f
  => [Record] -> [Record] -> f Record -> f Record
mkUserPosts' eSubs nSubs = fmap mkUserPost
  where
    eSubMap = mkFeedIndexedMap eSubs
    nSubMap = mkFeedIndexedMap nSubs
    getNSub post = fromJust $ Map.lookup (feedId post) nSubMap
    getESub post = fromJust $ Map.lookup (feedId post) eSubMap
    addRead e n r
      | e /= n = mergeRecords r $ Record ["read" =: True]
      | otherwise = r
    mkUserPost post =
      addRead (isEnabled $ getESub post) (isEnabled $ getNSub post) $
      Record
        [ "subscriptionId" =: getIdValue' (getNSub post)
        , "postId" =: getIdValue' post
        ]

-- ^
-- Make user posts for new subscriptions
-- TODO: remove
mkUserPosts :: [Record] -> [Record] -> [Record]
mkUserPosts subs = fmap mkUserPost
  where
    subMap = mkRecordMap "feedId" subs
    getSub post = fromJust $ Map.lookup (feedId post) subMap
    mkUserPost post =
      Record
        [ "subscriptionId" =: getIdValue' (getSub post)
        , "postId" =: getIdValue' post
        , "read" =: True
        ]

mkFeedIndexedMap :: [Record] -> Map.Map RecordId Record
mkFeedIndexedMap = mkRecordMap "feedId"

getUserPostIds :: Map.Map RecordId Record -> [Record] -> [RecordId]
getUserPostIds subMap = fmap mkId
  where
    getSubId post = getIdValue' $ fromJust $ Map.lookup (feedId post) subMap
    mkId post = mkUserPostId (getSubId post) (getIdValue' post)

getUserPostsBySubs
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => [Record] -> ExceptT ApiError m [Record]
getUserPostsBySubs subs = do
  search <- ExceptT . return $ mkSearchBySubs subs
  runEsAndExtract (E.searchDocuments search userPostCollection)

-- ^
-- Delete the subscription the given id and its related user-posts
deleteSubscription
  :: (MonadBaseControl IO m, MonadIO m)
  => RecordId -> (Database, Pipe) -> (Text, Text) -> ExceptT ApiError m ()
deleteSubscription subId (dbName, pipe) (server, index) = do
  _ <- getExistingSubOld subId dbName pipe
  deletePostsOld [subId] server index userPostCollection

-- TODO: consolidate with Handlers.Common#getExisting
-- TODO: remove
getExistingSubOld
  :: (MonadIO m, MonadBaseControl IO m)
  => RecordId -> Database -> Pipe -> ExceptT ApiError m Record
getExistingSubOld subId dbName pipe = do
  sub <- runDbOld (M.dbGetById subscriptionDefinition subId) dbName pipe
  ExceptT . return $ maybe (Left mkApiError404) Right sub

-- ^
-- Remove all user-posts associated with the given subscriptions
-- from the search index
deletePosts ::
  (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m) =>
  [Record] -> ExceptT ApiError m ()
deletePosts subs = do
  search <- ExceptT (return $ mkSearchBySubs subs)
  void . runEs $ E.deleteByQuery search userPostCollection

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
-- Delete the user-posts related to the input subscriptions from the search index
-- TODO: remove
deletePostsOld
  :: MonadIO m
  => [Text] -> Text -> Text -> Text -> ExceptT ApiError m ()
deletePostsOld subIds server index mappingName = do
  search <- ExceptT (return search')
  runEsOld (E.deleteByQuery search) mappingName server index >>= printLog
  where
    filter' = FilterRelOp In "subscriptionId" (TermList $ TermId <$> subIds)
    search' = first E.esToApiError $ E.mkSearch (Just filter') [] [] 0 0
    printLog _ = liftIO $ print $ trace "Delete user posts for" subIds

mkSubscriptions :: [Record] -> [Record] -> [Record]
mkSubscriptions feeds = fmap mkSub
  where
    feedMap = mkIdIndexedMap feeds
    mkSub r = setValue "title" (getTitle r $ getFeed r) r
    getFeed r = Map.lookup (feedId r) feedMap
    getTitle :: Record -> Maybe Record -> Text
    getTitle r f =
      fromJust $
      getValue "title" r <|> (f >>= getValue "title") <|> Just "Untitled"

-- ^
-- Get all posts related to all input subscriptions. The records
-- returned will only contain the id and the feedId fields.
-- TODO: remove
getPostsBySubOld
  :: (MonadBaseControl IO m, MonadIO m)
  => [Record] -> Database -> Pipe -> m (Either Failure [Record])
getPostsBySubOld subs = M.dbFind postDefinition query [] [] 0 0
  where
    query = M.idsQuery' "feedId" (feedId <$> subs)

-- ^
-- Get all posts related to all input subscriptions. The records
-- returned will only contain the id and the feedId fields.
getPostsBySub
  :: (MonadReader ApiConfig m, MonadBaseControl IO m, MonadIO m)
  => [Record] -> ExceptT ApiError m [Record]
getPostsBySub subs = runDb $ M.dbFind postDefinition query [] [] 0 0
  where
    query = M.idsQuery' "feedId" (feedId <$> subs)

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
  uid <- runDbOld (action def $ populateDefaults def input) dbName pipe
  record <- runDbOld (M.dbGetById def uid) dbName pipe
  return (fromJust record)

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
isEnabled = not . isEnabled