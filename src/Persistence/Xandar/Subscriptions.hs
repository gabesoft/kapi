{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- ^
-- Persistence functions for feed subscriptions
module Persistence.Xandar.Subscriptions where

import Control.Applicative ((<|>))
import Control.Monad.Except
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
import Persistence.Facade (validate, validateId)
import qualified Persistence.MongoDB as M
import Persistence.Xandar.Common
import Persistence.Xandar.UserPosts (insertUserPosts, mkUserPostId, indexDocuments)
import qualified Persistence.Xandar.UserPosts as U
import Types.Common

userPostMapping = recordCollection userPostDefinition

getSingleSubscription = undefined

getMultipleSubscriptions = undefined

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
  posts <- runDbOld (getPostsBySub $ filter isEnabled saved) dbName pipe
  _ <-
    insertUserPosts -- TODO: use custom index method since we have all data necessary
      True
      (mkUserPosts saved posts)
      (dbName, pipe)
      (server, index)
  return $ mkApiItems (Fail <$> lefts validated) (Succ <$> saved)
  where
    save r = saveDb M.dbInsert r dbName pipe

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
  deletePosts disabledIds server index userPostMapping
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

insertUserPosts'
  :: [Record]
  -> [Record]
  -> (Database, Pipe)
  -> (Text, Text)
  -> ExceptT ApiError IO [ApiItem ApiError Record]
insertUserPosts' eSubs nSubs (dbName, pipe) (server, index) = do
  let mapping = recordCollection userPostDefinition
  posts <- runDbOld (getPostsBySub $ filter isEnabled nSubs) dbName pipe
  let userPostIds = getUserPostIds (mkFeedIndexedMap nSubs) posts
  existing <- runEsAndExtractOld (E.getByIds userPostIds) mapping server index
  results <- mkUserPosts'' (eSubs, nSubs, posts) existing
  created <- indexDocuments (rights results) mapping server index
  return $ (Succ <$> created) <> (Fail <$> lefts results)

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
      addTags (getSub' r nSubMap) (getSub r eSubMap) (getExisting rid) r
    getSub' r = fromJust . getSub r
    getSub r = Map.lookup (getValue' "feedId" r)
    getExisting rid = Map.lookup rid existingMap

-- TODO: handle other cases
addTags :: Record -> Maybe Record -> Maybe Record -> Record -> Record
addTags _ Nothing _ r = r
addTags _ _ Nothing r = r
addTags nSub (Just eSub) (Just existing) r = setTags (computeTags eTags nTags $ getTags existing) r
  where
    getTags :: Record -> [Text]
    getTags = fromMaybe [] . getValue "tags"
    setTags = setValue "tags"
    eTags = getTags eSub
    nTags = getTags nSub

computeTags :: Ord a => [a] -> [a] -> [a] -> [a]
computeTags eTags nTags tags = Set.toList $ (tSet \\ eSet) <> nSet
  where eSet = Set.fromList eTags
        nSet = Set.fromList nTags
        tSet = Set.fromList tags

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
mkUserPosts
  :: Functor f
  => [Record] -> f Record -> f (RecordData Field)
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

-- ^
-- Delete the subscription the given id and its related user-posts
deleteSubscription
  :: (MonadBaseControl IO m, MonadIO m)
  => RecordId -> (Database, Pipe) -> (Text, Text) -> ExceptT ApiError m ()
deleteSubscription subId (dbName, pipe) (server, index) = do
  _ <- getExistingSub subId dbName pipe
  deletePosts [subId] server index userPostMapping

-- TODO: consolidate with Handlers.Common#getExisting
getExistingSub
  :: (MonadIO m, MonadBaseControl IO m)
  => RecordId -> Database -> Pipe -> ExceptT ApiError m Record
getExistingSub subId dbName pipe = do
  sub <- runDbOld (M.dbGetById subscriptionDefinition subId) dbName pipe
  ExceptT . return $ maybe (Left mkApiError404) Right sub

-- ^
-- Delete the user-posts related to the input subscriptions from the search index
deletePosts
  :: MonadIO m
  => [Text] -> Text -> Text -> Text -> ExceptT ApiError m ()
deletePosts subIds server index mappingName = do
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
getPostsBySub
  :: (MonadBaseControl IO m, MonadIO m)
  => [Record] -> Database -> Pipe -> m (Either Failure [Record])
getPostsBySub subs = M.dbFind postDefinition query [] [] 0 0
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
-- Return true if a subscription is enabled
isEnabled :: Record -> Bool
isEnabled = not . isValueOn "disabled"