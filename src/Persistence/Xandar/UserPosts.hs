{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Persistence functions for user-posts
module Persistence.Xandar.UserPosts where

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
  let mapping = recordCollection userPostDefinition
  let validated = validate' <$> input
  let valid = rights validated
  let invalid = lefts validated
  existing <- runEs' (E.getByIds $ mkUserPostId' <$> valid) server index mapping
  subs <- runDb (getSubsById $ subId <$> valid) dbName pipe
  posts <- runDb (getPostsById $ postId <$> valid) dbName pipe
  results <- mkUserPosts replace (subs, posts) (existing, valid)
  created <- indexDocuments (rights results) server index mapping
  return $ (Succ <$> created) <> (Fail <$> lefts results <> invalid)

-- ^
-- Index multiple documents and re-query them
indexDocuments :: [(Record, RecordId)]
               -> Text
               -> Text
               -> Text
               -> ExceptT ApiError IO [Record]
indexDocuments [] _ _ _ = return []
indexDocuments input server index mapping = do
  _ <- runEs (E.indexDocuments input) server index mapping >>= printLog
  _ <- runEs refreshIndex server index mapping
  runEs' (E.getByIds $ snd <$> input) server index mapping
  where
    refreshIndex s i _ = E.refreshIndex s i
    printLog msg = liftIO $ print $ trace "Insert user posts" msg

-- ^
-- Construct user post documents
mkUserPosts
  :: (MonadIO m)
  => Bool
  -> ([Record], [Record])
  -> ([Record], [Record])
  -> m [Either ApiError (Record, RecordId)]
mkUserPosts replace (subs, posts) (existing, input) = mapM mkRecord input
  where
    mkRecord r = mkUserPost replace (getSubPost r) (findExisting r, r)
    getSubPost r = (get subMap subId r, get postMap postId r)
    findExisting = get existingMap mkUserPostId'
    get m fid r = Map.lookup (fid r) m
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
  | otherwise = Right . flip (,) recId <$> addTimestamp existing record
  where
    (recId, ids) = getIds sub post
    record = mergeRecords (mkRecord replace existing) input'
    sub' = includeFields ["title", "notes", "tags"] sub
    base = foldr (uncurry setValue) (mergeRecords sub' $ mkPost post) ids
    mkRecord _ Nothing = base
    mkRecord True _ = base
    mkRecord False (Just existing) = excludeFields [idLabel] existing
    input' =
      excludeFields
        ["post", "userId", "feedId", createdAtLabel, updatedAtLabel, idLabel]
        input

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
mkUserPostId' :: Record -> RecordId
mkUserPostId' record =
  mkUserPostId (getValue' "subscriptionId" record) (getValue' "postId" record)

-- ^
-- Generate an id given a subscription id and a post id
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

-- ^
-- Get the subscription id of a user-post
subId :: Record -> RecordId
subId = getValue' "subscriptionId"

-- ^
-- Get the post id of a user-post
postId :: Record -> RecordId
postId = getValue' "postId"

runDb
  :: (MonadBaseControl IO m)
  => (Database -> Pipe -> m (Either Failure c))
  -> Database
  -> Pipe
  -> ExceptT ApiError m c
runDb action dbName pipe = do
  results <- lift $ action dbName pipe
  ExceptT (return $ first M.dbToApiError results)

runEs'
  :: Monad m
  => (a -> b -> c -> m (Either EsError (SearchResult Record)))
  -> a
  -> b
  -> c
  -> ExceptT ApiError m [Record]
runEs' action server index mappingName = do
  results <- runEs action server index mappingName
  return $ E.extractRecords [] results

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
mkRecordMap :: [Record] -> Map.Map RecordId Record
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
