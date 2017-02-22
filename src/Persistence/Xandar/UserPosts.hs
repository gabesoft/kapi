{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Persistence functionality for user-posts
module Persistence.Xandar.UserPosts
  ( esInsert
  , esInsertMulti
  , esUpdate
  , esUpdateMulti
  , indexDocuments
  , indexDocumentsOld
  , insertUserPosts
  , mkUserPostId
  , mkUserPost
  , mkUserPosts
  ) where

import Control.Monad.Except
import Control.Monad.Reader
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
import Network.HTTP.Types
import Persistence.Common
import qualified Persistence.ElasticSearch as E
import Persistence.Facade
       (validate, validateId, validateMulti, validateEsIdMulti,
        getExistingMulti, runEs, runEsAndExtract, toSingle)
import qualified Persistence.MongoDB as M
import Persistence.Xandar.Common
import Types.Common

-- ^
-- Insert new user posts or replace existing ones
insertUserPosts
  :: Bool
  -> [Record]
  -> (Database, Pipe)
  -> (Text, Text)
  -> ExceptT ApiError IO ApiResults
insertUserPosts replace input (dbName, pipe) (server, index) = do
  let mapping = recordCollection userPostDefinition
  let validated = validateUserPost <$> input -- TODO for update this should not fail on post id missing
  let valid = rights validated
  let invalid = lefts validated
  let userPostIds = mkUserPostId' <$> valid
  existing <- runEsAndExtractOld (E.getByIds userPostIds) mapping server index
  subs <- runDbOld (getSubsById $ subId <$> valid) dbName pipe
  posts <- runDbOld (getPostsById $ postId <$> valid) dbName pipe
  results <- mkUserPosts replace (subs, posts) (existing, valid)
  created <- indexDocumentsOld (rights results) mapping server index
  return $ mkApiItems (Fail <$> lefts results <> invalid) (Succ <$> created)

esInsert
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => Record -> ExceptT ApiError m Record
esInsert = toSingle esInsertMulti

esUpdate
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => Bool -> Record -> ExceptT ApiError m Record
esUpdate replace = toSingle (esUpdateMulti replace)

esInsertMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [Record] -> ApiItems2T [ApiError] m [Record]
esInsertMulti input = do
  valid <- validateMulti userPostDefinition input
  subs <- getExistingMulti subscriptionDefinition (subId <$> valid)
  posts <- getExistingMulti postDefinition (postId <$> valid)
  records <- mkNewUserPosts (subs, posts) valid
  indexDocuments records

esUpdateMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => Bool -> [Record] -> ApiItems2T [ApiError] m [Record]
esUpdateMulti replace input = do
  valid1 <- validateEsIdMulti input
  undefined -- TODO: left here

indexDocuments
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [(Record, RecordId)] -> ApiItems2T [ApiError] m [Record]
indexDocuments input = do
  let mapping = recordCollection userPostDefinition
  _ <- runExceptT $ runEs (E.indexDocuments input mapping)
  _ <- runExceptT $ runEs E.refreshIndex
  records <-
    runExceptT $ runEsAndExtract (E.getByIds (snd <$> input) mapping)
  ApiItems2T . return $
    either (flip ApiItems2 [] . (: [])) (ApiItems2 []) records

-- ^
-- Index multiple documents and re-query them
indexDocumentsOld :: [(Record, RecordId)]
               -> Text
               -> Text
               -> Text
               -> ExceptT ApiError IO [Record]
indexDocumentsOld [] _ _ _ = return []
indexDocumentsOld input mapping server index = do
  _ <- runEsOld (E.indexDocuments input) mapping server index >>= printLog
  _ <- runEsOld refreshIndex mapping server index
  runEsAndExtractOld (E.getByIds $ snd <$> input) mapping server index
  where
    refreshIndex _ = E.refreshIndex
    printLog _ = liftIO $ print $ trace "Insert user posts" (length input)

mkNewUserPosts
  :: (MonadIO m)
  => ([Record], [Record])
  -> [Record]
  -> ApiItems2T [ApiError] m [(Record, RecordId)]
mkNewUserPosts (subs, posts) input = mkUserPosts' True (subs, posts) ([], input)

mkUserPosts'
  :: (MonadIO m)
  => Bool
  -> ([Record], [Record])
  -> ([Record], [Record])
  -> ApiItems2T [ApiError] m [(Record, RecordId)]
mkUserPosts' replace (subs, posts) (existing, input) = do
  records <- mapM mkRecord input
  ApiItems2T . return $ eitherToItems records
  where
    mkRecord r = mkUserPost replace (getSubPost r) (findExisting r, r)
    getSubPost r = (get subMap subId r, get postMap postId r)
    findExisting = get existingMap mkUserPostId'
    get m fid r = Map.lookup (fid r) m
    existingMap = mkIdIndexedMap existing
    subMap = mkIdIndexedMap subs
    postMap = mkIdIndexedMap posts

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
    existingMap = mkIdIndexedMap existing
    subMap = mkIdIndexedMap subs
    postMap = mkIdIndexedMap posts

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
    mkRecord False (Just e) = excludeFields [idLabel] e
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
    subId' = lookup' "subscriptionId" output
    postId' = lookup' "postId" output
    recId = mkUserPostId subId' postId'

-- ^
-- Generate an id for a user post
mkUserPostId' :: Record -> RecordId
mkUserPostId' record =
  mkUserPostId (getValue' "subscriptionId" record) (getValue' "postId" record)

-- ^
-- Generate an id given a subscription id and a post id
mkUserPostId :: RecordId -> RecordId -> RecordId
mkUserPostId subId' postId' = subId' <> "-" <> postId'

-- ^
-- Get multiple posts by id
getPostsById
  :: (MonadBaseControl IO m, MonadIO m)
  => [RecordId] -> Database -> Pipe -> m (Either Failure [Record])
getPostsById = getRecordsById postDefinition

-- ^
-- Get multiple subscriptions by id
getSubsById
  :: (MonadBaseControl IO m, MonadIO m)
  => [RecordId] -> Database -> Pipe -> m (Either Failure [Record])
getSubsById = getRecordsById subscriptionDefinition

-- ^
-- Get the subscription id of a user-post
subId :: Record -> RecordId
subId = getValue' "subscriptionId"

-- ^
-- Get the post id of a user-post
postId :: Record -> RecordId
postId = getValue' "postId"

-- ^
-- Make a 400 error to be returned when attempting to construct an invalid user post
mk400Err :: String -> Record -> ApiError
mk400Err msg record = ApiError (Just record) status400 (LBS.pack msg)
