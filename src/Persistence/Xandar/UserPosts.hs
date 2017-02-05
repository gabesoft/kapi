{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Persistence functions for user-posts
module Persistence.Xandar.UserPosts where

import Control.Applicative (liftA2)
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import qualified Data.Aeson as A
import Data.Bifunctor
import Data.Bson hiding (lookup)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either
import Data.Function ((&))
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid ((<>))
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Database.Bloodhound (EsError(..))
import Database.MongoDB (Database, Pipe, Failure)
import Debug.Trace
import Persistence.Common
import qualified Persistence.ElasticSearch as E
import qualified Persistence.MongoDB as M
import Persistence.Xandar.FeedSubscriptions
       (feedSubscriptionDefinition)
import Persistence.Xandar.Posts (postDefinition)
import Types.Common

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
-- Insert new user posts
insertUserPosts :: [Record]
                -> (Database, Pipe)
                -> (Text, Text)
                -> ExceptT ApiError IO [ApiResult]
insertUserPosts input (dbName, pipe) (server, index) = do
  valid <- mapM validate' input
  let subIds = vals "subscriptionId" valid
  let postIds = vals "postId" valid
  subs <- runDb (getSubsById subIds) dbName pipe
  posts <- runDb (getPostsById postIds) dbName pipe
  let results = mkUserPosts (subs, posts) valid
  let failed = lefts results
  created <- indexDocuments (rights results) server index mapping
  return $ (Succ <$> created) <> (Fail <$> failed)
  where
    vals label = catMaybes . fmap (getValue label)
    mapping = recordCollection userPostDefinition

-- ^
-- Index multiple documents and re-query them
indexDocuments :: [(Record, RecordId)]
               -> Text
               -> Text
               -> Text
               -> ExceptT ApiError IO [Record]
indexDocuments [] _ _ _ = return []
indexDocuments input server index mapping = do
  existing <- runEs (E.getByIds ids) server index mapping
  let existingMap = mkRecordMap (E.extractRecords [] existing)
  items <- liftIO $ mapM (mergeUserPosts existingMap) input
  runEs (E.indexDocuments items) server index mapping >>= log
  runEs refreshIndex server index mapping
  created <- runEs (E.getByIds ids) server index mapping
  return (E.extractRecords [] created)
  where
    ids = snd <$> input
    refreshIndex s i _ = E.refreshIndex s i
    log msg = liftIO $ print $ trace "Insert user posts" msg

-- ^
-- Merge an existing with a new user post
-- TODO: merge tags
mergeUserPosts :: MonadIO m => Map.Map RecordId Record -> (Record, RecordId) -> m (Record, RecordId)
mergeUserPosts recMap (new, recId) = mergeDates existingDate
  where
    existing = Map.lookup recId recMap
    existingDate :: Maybe Text
    existingDate = existing >>= getValue "createdAt"
    mergeDates Nothing = do
      record <- setTimestamp True new
      return (record, recId)
    mergeDates (Just createdAt) = do
      record <- setTimestamp False new
      return (setValue "createdAt" createdAt record, recId)

-- ^
-- Update existing user posts
updateUserPosts = insertUserPosts

-- ^
-- Construct user post documents
mkUserPosts :: ([Record], [Record])
            -> [Record]
            -> [Either ApiError (Record, RecordId)]
mkUserPosts (subs, posts) records = process <$> records
  where
    process r = maybe (Left $ mkErr r) Right (build r)
    build r = liftA2 (curry $ flip mkUserPost r) (findSub r) (findPost r)
    mkErr r =
      mkApiError400 $
      unwords $
      filter
        (not . null)
        [ msg "Subscription" (subId r) (findSub r)
        , msg "Post" (postId r) (findPost r)
        ]
    msg m rid = maybe (m ++ " " ++ T.unpack rid ++ " not found.") mempty
    findSub record = Map.lookup (subId record) subMap
    findPost record = Map.lookup (postId record) postMap
    subId = getId "subscriptionId"
    postId = getId "postId"
    subMap = mkRecordMap subs
    postMap = mkRecordMap posts
    getId :: Label -> Record -> RecordId
    getId label = fromJust . getValue label

-- ^
-- Make a make a map with the ids as keys and records as values
mkRecordMap xs = Map.fromList (addId <$> xs)
  where
    addId r = (fromJust $ getIdValue r, r)

-- ^
-- Combine a subscription, post, and input record to create a user-post ready to be indexed
mkUserPost :: (Record, Record) -> Record -> (Record, RecordId)
mkUserPost (sub, post) record = (build record, recId)
  where
    build r = foldr set (flip mergeRecords post' $ mergeRecords sub' r) overwriteIds
    (recId, overwriteIds) = getIds sub post
    set (name, val) = setValue name val
    sub' = includeFields subFields sub
    post' = Record ["post" =: getDocument (excludeFields postSkipFields post)]
    subFields = ["title", "notes", "tags"]
    postSkipFields = ["feedId", "_id", "pubdate", "__v"]

-- ^
-- Get all ids required to create a user-post record
getIds :: Record -> Record -> (RecordId, [(RecordId, RecordId)])
getIds sub post = (recId, output)
  where
    output = get <$> input
    input =
      [ ("feedId", "feedId", sub)
      , ("userId", "userId", sub)
      , ("subscriptionId", "_id", sub)
      , ("postId", "_id", post)
      ]
    get (outLabel, label, r) = (outLabel, fromJust $ getValue label r)
    subId = fromJust $ lookup "subscriptionId" output
    postId = fromJust $ lookup "postId" output
    recId = mkUserPostId subId postId

-- ^
-- Generate an id for a user post
mkUserPostId
  :: (Monoid m, IsString m)
  => m -> m -> m
mkUserPostId subId postId = subId <> "-" <> postId

-- ^
-- Get multiple posts by id
getPostsById
  :: (MonadBaseControl IO m, MonadIO m)
  => [RecordId] -> Database -> Pipe -> m (Either Failure [Record])
getPostsById ids = M.dbFind postDefinition (M.idsQuery ids) [] [] 0 0

-- ^
-- Get multiple subscriptions by id
getSubsById
  :: (MonadBaseControl IO m, MonadIO m)
  => [RecordId] -> Database -> Pipe -> m (Either Failure [Record])
getSubsById ids = M.dbFind feedSubscriptionDefinition (M.idsQuery ids) [] [] 0 0

runDb
  :: (MonadBaseControl IO m)
  => (Database -> Pipe -> m (Either Failure c))
  -> Database
  -> Pipe
  -> ExceptT ApiError m c
runDb action dbName pipe = do
  results <- lift $ action dbName pipe
  ExceptT (return $ first M.dbToApiError results)

runEs
  :: Monad m
  => (a -> b -> c -> m (Either EsError d))
  -> a
  -> b
  -> c
  -> ExceptT ApiError m d
runEs action server index mappingName = do
  results <- lift $ action server index mappingName
  ExceptT (return $ first E.esToApiError results)

validate'
  :: Monad m
  => Record -> ExceptT ApiError m Record
validate' record =
  ExceptT . return $ vResultToEither (validate userPostDefinition record)
