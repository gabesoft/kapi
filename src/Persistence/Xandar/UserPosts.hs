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
  => [Text] -> Database -> Pipe -> m (Either Failure [Record])
getPostsById ids = M.dbFind postDefinition query [] [] 0 0
  where
    query = ["_id" =: ("$in" =: ids)]

-- ^
-- Get a subscription by id
getSubscription
  :: (MonadBaseControl IO m, MonadIO m)
  => RecordId -> Database -> Pipe -> m (Either Failure (Maybe Record))
getSubscription = M.dbGetById feedSubscriptionDefinition

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
