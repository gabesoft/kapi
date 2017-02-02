{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Persistence functions for user-posts
module Persistence.Xandar.UserPosts where

import Control.Applicative (liftA2)
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Bifunctor
import Data.Bson
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
                -> ExceptT ApiError IO [ApiResult]
insertUserPosts input dbName pipe server index = do
  valid <- mapM validate' input
  let subIds = vals "subscriptionId" valid
  let postIds = vals "postId" valid
  subs <- runDb (getSubsById subIds) dbName pipe
  posts <- runDb (getPostsById postIds) dbName pipe
  let results = mkUserPosts subs posts valid
  let failed = lefts results
  let items = rights results
  _ <- runEs (E.indexDocuments items) server index mapping
  let recIds = snd <$> items
  created <- runEs (E.getByIds recIds) server index mapping
  return $ (Succ <$> E.extractRecords [] created) <> (Fail <$> failed)
  where
    vals label = catMaybes . fmap (getValue label)
    mapping = recordCollection userPostDefinition

-- ^
-- Update existing user posts
updateUserPosts input = undefined

-- ^
-- Construct user post documents
mkUserPosts :: [Record] -> [Record] -> [Record] -> [Either ApiError (Record, RecordId)]
mkUserPosts subs posts records = process <$> records
  where
    process r = maybe (Left $ mkErr r) Right (build r)
    build r = liftA2 (mkUserPost r) (findSub r) (findPost r)
    mkErr r =
      mkApiError400 $
      unwords
        [ msg "Subscription" (subId r) (findSub r)
        , msg "Post" (postId r) (findPost r)
        ]
    msg m rid = maybe (m ++ T.unpack rid ++ "not found.") mempty
    findSub record = Map.lookup (subId record) subMap
    findPost record = Map.lookup (postId record) postMap
    subId = getId "subscriptionId"
    postId = getId "postId"
    subMap = mkMap subs
    postMap = mkMap posts
    getId :: Label -> Record -> RecordId
    getId label = fromJust . getValue label
    addId r = (fromJust $ getIdValue r, r)
    mkMap xs = Map.fromList (addId <$> xs)

mkUserPost :: Record -> Record -> Record -> (Record, RecordId)
mkUserPost record sub post = (build record, recId)
  where
    build =
      setValue "subscriptionId" subId .
      setValue "postId" postId . flip mergeRecords post' . mergeRecords sub'
    sub' = includeFields subFields sub
    post' = Record ["post" =: getDocument (excludeFields postSkipFields post)]
    subId = fromJust $ getIdValue sub
    postId = fromJust $ getIdValue post
    recId = mkUserPostId subId postId
    subFields = ["userId", "feedId", "title", "notes", "tags"]
    postSkipFields = ["feedId", "_id", "pubdate", "__v"]

-- ^
-- Generate an id for a user post
mkUserPostId :: (Monoid m, IsString m) => m -> m -> m
mkUserPostId subId postId = subId <> "-" <> postId

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
