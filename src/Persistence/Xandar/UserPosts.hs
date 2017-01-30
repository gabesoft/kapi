{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Persistence functions for user-posts
module Persistence.Xandar.UserPosts where

import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Bifunctor
import Data.Bson
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid ((<>))
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Database.Bloodhound (EsError(..))
import Database.MongoDB (Database, Pipe, Failure)
import Persistence.Common
import qualified Persistence.ElasticSearch as E
import qualified Persistence.MongoDB as M
import Persistence.Xandar.FeedSubscriptions
       (feedSubscriptionDefinition)
import Persistence.Xandar.Posts (postDefinition)
import Types.Common

userPostDefinition :: RecordDefinition
userPostDefinition =
  RecordDefinition "posts" $
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
                -> Database
                -> Pipe
                -> Text
                -> Text
                -> ExceptT ApiError IO b
insertUserPosts input dbName pipe server index = do
  valid <- mapM validate' input
  let subIds = vals "subscriptionId" valid
  let postIds = vals "postId" valid
  subs <- runDb (getSubsById subIds) dbName pipe
  posts <- runDb (getPostsById postIds) dbName pipe
  let items = mkUserPosts subs posts valid
  _ <- runEs (E.indexDocuments items) server index mapping
  let recIds = snd <$> items
  created <- runEs (E.getByIds recIds) server index mapping
  return $ extractRecords created
  where
    vals label = catMaybes . fmap (getValue label)
    mapping = recordCollection userPostDefinition

-- ^
-- Update existing user posts
updateUserPosts input = undefined

-- ^
-- Construct user post documents
mkUserPosts :: [Record] -> [Record] -> [Record] -> [(Record, Text)]
mkUserPosts subs posts records = undefined
  where
    subMap = mkMap subs
    postMap = mkMap posts
    addId r = (fromJust $ getIdValue r, r)
    mkMap xs = Map.fromList (addId <$> xs)

extractRecords search = undefined

-- ^
-- Generate an id for a user post
mkUserPostId
  :: (Val a, Monoid a, IsString a)
  => Record -> Maybe a
mkUserPostId record =
  getValue "postId" record <> Just "-" <> getValue "subscriptionId" record

-- ^
-- Get multiple posts by id
getPostsById
  :: (MonadBaseControl IO m, MonadIO m)
  => [RecordId] -> Database -> Pipe -> m (Either Failure [Record])
getPostsById ids = M.dbFind postDefinition (idsQuery ids) [] [] 0 0

-- ^
-- Get multiple subscriptions by id
getSubsById
  :: (MonadBaseControl IO m, MonadIO m)
  => [RecordId] -> Database -> Pipe -> m (Either Failure [Record])
getSubsById ids =
  M.dbFind feedSubscriptionDefinition (idsQuery ids) [] [] 0 0

idsQuery ids = ["_id" =: ("$in" =: ids)]

runDb
  :: (MonadBaseControl IO m, MonadIO m)
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
