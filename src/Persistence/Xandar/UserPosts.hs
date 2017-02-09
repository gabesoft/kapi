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
-- Insert new user posts or replace existing ones
insertUserPosts
  :: Bool
  -> [Record]
  -> (Database, Pipe)
  -> (Text, Text)
  -> ExceptT ApiError IO [ApiResult]
insertUserPosts replace input (dbName, pipe) (server, index) = do
  let vResults = validate' <$> input
  let valid = rights vResults
  let invalid = lefts vResults
  let subIds = vals "subscriptionId" valid
  let postIds = vals "postId" valid
  let ids = mkId <$> valid
  existingResults <- runEs (E.getByIds ids) server index mapping
  subs <- runDb (getSubsById subIds) dbName pipe
  posts <- runDb (getPostsById postIds) dbName pipe
  let existing = E.extractRecords [] existingResults
  results <- mkUserPosts replace (subs, posts) (existing, valid)
  let failed = lefts results
  created <- indexDocuments (rights results) server index mapping
  return $ (Succ <$> created) <> (Fail <$> failed) <> (Fail <$> invalid)
  where
    vals label = catMaybes . fmap (getValue label)
    mapping = recordCollection userPostDefinition
    getId label = fromJust . getValue label
    mkId r = mkUserPostId (getId "subscriptionId" r) (getId "postId" r)

-- ^
-- Index multiple documents and re-query them
indexDocuments :: [(Record, RecordId)]
               -> Text
               -> Text
               -> Text
               -> ExceptT ApiError IO [Record]
indexDocuments [] _ _ _ = return []
indexDocuments input server index mapping = do
  runEs (E.indexDocuments input) server index mapping >>= log
  runEs refreshIndex server index mapping
  created <- runEs (E.getByIds ids) server index mapping
  return (E.extractRecords [] created)
  where
    ids = snd <$> input
    refreshIndex s i _ = E.refreshIndex s i
    log msg = liftIO $ print $ trace "Insert user posts" msg

-- ^
-- Construct user post documents
mkUserPosts
  :: (MonadIO m)
  => Bool
  -> ([Record], [Record])
  -> ([Record], [Record])
  -> m [Either ApiError (Record, RecordId)]
mkUserPosts replace (subs, posts) (existing, input) = mapM build input
  where
    build r = mkRecord r (findSub r) (findPost r)
    mkRecord r s p = mkUserPost replace (s, p) (findExisting s p, r)
    findSub record = Map.lookup (subId record) subMap
    findPost record = Map.lookup (postId record) postMap
    findExisting s p = liftA2 mkId s p >>= flip Map.lookup existingMap
    mkId s p = mkUserPostId (getIdValue' s) (getIdValue' p)
    subId = getValue' "subscriptionId"
    postId = getValue' "postId"
    existingMap = mkRecordMap existing
    subMap = mkRecordMap subs
    postMap = mkRecordMap posts

-- ^
-- Given a subscription, post, input data, and possibly an
-- existing user-post create a user-post ready to be indexed
mkUserPost
  :: (MonadIO m)
  => Bool
  -> (Maybe Record, Maybe Record)
  -> (Maybe Record, Record)
  -> m (Either ApiError (Record, RecordId))
mkUserPost _ (Nothing, Nothing) (_, r) =
  return . Left $ mk400Err "Subscription and post not found." r
mkUserPost _ (Nothing, _) (_, r) =
  return . Left $ mk400Err "Subscription not found." r
mkUserPost _ (_, Nothing) (_, r) = return . Left $ mk400Err "Post not found." r
mkUserPost replace (Just sub, Just post) (existing, input)
  | getValue' "feedId" sub /= (getValue' "feedId" post :: RecordId) =
    return . Left $ mk400Err "Post belongs to a different subscription." input
  | otherwise = do
    record <- addTimestamp existing mkRecord
    return $ Right (record, recId)
  where
    (recId, ids) = getIds sub post
    mkRecord = mergeRecords base (clean input)
    mkBase = foldr (uncurry setValue) (mergeRecords sub' $ mkPost post) ids
    base = baseUserPost replace existing mkBase
    sub' = includeFields ["title", "notes", "tags"] sub
    clean =
      excludeFields
        ["post", "userId", "feedId", createdAtLabel, updatedAtLabel, idLabel]

-- ^
-- Return the base data for a user-post according to the 'replace' flag
baseUserPost :: Bool -> Maybe Record -> Record -> Record
baseUserPost _ Nothing base = base
baseUserPost replace (Just existing) base
  | replace = base
  | otherwise = excludeFields [idLabel] existing

-- ^
-- Return the value of the 'post' field of a user-post
mkPost :: Record -> Record
mkPost input = Record ["post" =: getDocument post]
  where
    post = excludeFields skipFields input
    skipFields = ["feedId", idLabel, "pubdate", "__v"]

-- ^
-- Add time-stamp dates to a user post record
addTimestamp
  :: (MonadIO m)
  => Maybe Record -> Record -> m Record
addTimestamp existing new = mergeDates new existingDate
  where
    existingDate :: Maybe Text
    existingDate = existing >>= getValue createdAtLabel
    mergeDates r Nothing = setTimestamp True r
    mergeDates r (Just createdAt) = do
      record <- setTimestamp False r
      return (setValue createdAtLabel createdAt record)

-- ^
-- Get all ids required to create a user-post record
getIds :: Record -> Record -> (RecordId, [(RecordId, RecordId)])
getIds sub post = (recId, output)
  where
    output = get <$> input
    input =
      [ ("feedId", "feedId", sub)
      , ("userId", "userId", sub)
      , ("subscriptionId", idLabel, sub)
      , ("postId", idLabel, post)
      ]
    get (outLabel, label, r) = (outLabel, fromJust $ getValue label r)
    subId = lookup' "subscriptionId" output
    postId = lookup' "postId" output
    recId = mkUserPostId subId postId

-- ^
-- Generate an id for a user post
mkUserPostId :: RecordId -> RecordId -> RecordId
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

validate' :: Record -> Either ApiError Record
validate' record = vResultToEither (validate userPostDefinition record)

-- ^
-- Make a make a map with the ids as keys and records as values
mkRecordMap xs = Map.fromList (addId <$> xs)
  where
    addId r = (fromJust $ getIdValue r, r)

-- ^
-- Make a 400 error to be returned when attempting to construct an invalid user post
mk400Err :: String -> Record -> ApiError
mk400Err msg record =
  mkApiError400 $ msg <> " Original input: " <> LBS.unpack (A.encode record)

lookup'
  :: Eq a
  => a -> [(a, c)] -> c
lookup' name = fromJust . lookup name
