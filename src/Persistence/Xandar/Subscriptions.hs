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
import qualified Database.Bloodhound as B
import Database.MongoDB (Database, Pipe, Failure, Index(..))
import Debug.Trace
import Persistence.Common
import Persistence.ElasticSearch
import qualified Persistence.ElasticSearch as E
import qualified Persistence.MongoDB as M
import Persistence.Xandar.Common
import Persistence.Xandar.UserPosts (insertUserPosts)
import Types.Common

userPostMapping = recordCollection userPostDefinition

getSingleSubscription = undefined

getMultipleSubscriptions = undefined

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
updateSubscriptions
  :: Bool
  -> [Record]
  -> (Database, Pipe)
  -> (Text, Text)
  -> ExceptT ApiError IO [ApiItem ApiError Record]
updateSubscriptions replace input (dbName, pipe) (server, index) = do
  let validated = validateHasId' <$> input
  let valid = rights validated
  existing <- runDb (getSubsById $ getIdValue' <$> valid) dbName pipe
  let existingMap = mkRecordMap existing
  let records = mkRecord existingMap <$> valid
  saved <- mapM save (rights records)
  posts <- runDb (getPostsBySub $ filter isEnabled saved) dbName pipe
  -- index user-posts of enabled subscriptions
  _ <-
    insertUserPosts
      True
      (mkUserPosts' existing saved posts)
      (dbName, pipe)
      (server, index)
  -- delete user-posts of disabled subscriptions
  let disabledIds = getIdValue' <$> filter (not . isEnabled) saved
  deletePosts disabledIds server index userPostMapping
  -- TODO: update tags
  return $ (Succ <$> saved) <> (Fail <$> lefts validated <> lefts records)
  where
    save r = saveDb M.dbUpdate r dbName pipe
    get m r = Map.lookup (getIdValue' r) m
    mkRecord existingMap r = mkRecord' (get existingMap r) r
    mkRecord' Nothing r = Left mkApiError404
    mkRecord' (Just e) r = Right $ mergeRecords' replace e r

-- ^
-- Delete the subscription the given id and its related user-posts
deleteSubscription
  :: (MonadBaseControl IO m, MonadIO m)
  => RecordId -> (Database, Pipe) -> (Text, Text) -> ExceptT ApiError m ()
deleteSubscription subId (dbName, pipe) (server, index) = do
  sub <- getExistingSub subId dbName pipe
  deletePosts [subId] server index userPostMapping

-- TODO: consolidate with Handlers.Common#getExisting
getExistingSub
  :: (MonadIO m, MonadBaseControl IO m)
  => RecordId -> Database -> Pipe -> ExceptT ApiError m Record
getExistingSub subId dbName pipe = do
  sub <- runDb (M.dbGetById subscriptionDefinition subId) dbName pipe
  ExceptT . return $ maybe (Left mkApiError404) Right sub

-- ^
-- Delete the user-posts related to the input subscriptions from the search index
deletePosts
  :: MonadIO m
  => [Text] -> Text -> Text -> Text -> ExceptT ApiError m ()
deletePosts subIds server index mappingName = do
  search <- ExceptT (return search')
  runEs (E.deleteByQuery search) server index mappingName >>= printLog
  where
    filter' = FilterRelOp In "subscriptionId" (TermList $ TermId <$> subIds)
    search' = first esToApiError $ mkSearch (Just filter') [] [] 0 0
    printLog _ = liftIO $ print $ trace "Delete user posts for" subIds

-- TODO: consolidate the two mkUserPosts functions
mkUserPosts'
  :: Functor f
  => [Record] -> [Record] -> f Record -> f Record
mkUserPosts' eSubs nSubs = fmap mkUserPost
  where
    eSubMap = mkRecordMap' "feedId" eSubs
    nSubMap = mkRecordMap' "feedId" nSubs
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

mkUserPosts
  :: Functor f
  => [Record] -> f Record -> f (RecordData Field)
mkUserPosts subs = fmap mkUserPost
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
