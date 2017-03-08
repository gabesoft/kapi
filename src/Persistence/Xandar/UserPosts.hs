{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Persistence functionality for user-posts
module Persistence.Xandar.UserPosts
  ( deleteUserPost
  , deleteUserPosts
  , indexUserPosts
  , indexUserPosts'
  , insertUserPost
  , insertUserPosts
  , mkUserPostId
  , mkUserPostId'
  , mkUserPostOnCreate
  , mkUserPostOnModify
  , mkUserPostOnReplace
  , mkUserPostsOnCreate
  , mkUserPostsOnModify
  , mkUserPostsOnReplace
  , modifyUserPost
  , modifyUserPosts
  , replaceUserPost
  , replaceUserPosts
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Bifunctor
import Data.Bson hiding (lookup, label)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (UTCTime)
import Persistence.Common
import qualified Persistence.ElasticSearch as E
import Persistence.Facade
import Persistence.Xandar.Common
import Types.Common
import Util.Constants
import Util.Error

insertUserPost
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => Record -> ExceptT ApiError m Record
insertUserPost = toSingle insertUserPosts

replaceUserPost
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => Record -> ExceptT ApiError m Record
replaceUserPost = toSingle replaceUserPosts

modifyUserPost
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => Record -> ExceptT ApiError m Record
modifyUserPost = toSingle modifyUserPosts

deleteUserPost
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => RecordId -> ExceptT ApiError m Record
deleteUserPost = toSingle deleteUserPosts

replaceUserPosts
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [Record] -> ApiItemsT [ApiError] m [Record]
replaceUserPosts = updateUserPosts mkUserPostsOnReplace

modifyUserPosts
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [Record] -> ApiItemsT [ApiError] m [Record]
modifyUserPosts = updateUserPosts mkUserPostsOnModify

insertUserPosts
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [Record] -> ApiItemsT [ApiError] m [Record]
insertUserPosts input = do
  valid <- validateUserPosts input
  subs <- dbGetExistingMulti subscriptionDefinition (subId <$> valid)
  posts <- dbGetExistingMulti postDefinition (postId <$> valid)
  records <- mkUserPostsOnCreate subs posts valid
  indexUserPosts records

updateUserPosts
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => ([Record] -> [Record] -> ApiItemsT [ApiError] m [(Record, RecordId)])
  -> [Record]
  -> ApiItemsT [ApiError] m [Record]
updateUserPosts update input = do
  valid1 <- validateEsIdMulti input
  existing <- toMulti (getUserPosts $ getIdValue' <$> valid1)
  records <- update existing valid1
  valid2 <- validateMulti' fst validateRecordTuple records
  indexUserPosts valid2

-- ^
-- Delete multiple records by id
deleteUserPosts
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => [RecordId] -> ApiItemsT [ApiError] m [Record]
deleteUserPosts ids = do
  existing <- getExistingUserPosts ids
  _ <- runExceptT $ runEs (E.deleteByIds ids userPostCollection)
  return existing

-- ^
-- Get multiple records by id
getExistingUserPosts
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [RecordId] -> ApiResultsT m
getExistingUserPosts = esGetExistingMulti userPostDefinition

-- ^
-- Index multiple documents and re-query them
indexUserPosts
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => [(Record, RecordId)] -> ApiResultsT m
indexUserPosts input =
  runExceptT (indexUserPosts' input) >> getExistingUserPosts (snd <$> input)

-- ^
-- Index multiple documents
indexUserPosts'
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => [(Record, RecordId)] -> ExceptT ApiError m Text
indexUserPosts' [] = return mempty
indexUserPosts' input =
  runEs (E.indexDocuments input userPostCollection) <* runEs E.refreshIndex

mkUserPostsOnReplace
  :: (MonadIO m)
  => [Record] -> [Record] -> ApiItemsT [ApiError] m [(Record, RecordId)]
mkUserPostsOnReplace = mkUserPostsOnUpdate mkUserPostOnReplace

mkUserPostsOnModify
  :: (MonadIO m)
  => [Record] -> [Record] -> ApiItemsT [ApiError] m [(Record, RecordId)]
mkUserPostsOnModify = mkUserPostsOnUpdate mkUserPostOnModify

mkUserPostsOnUpdate
  :: Monad m
  => (Maybe Record -> Record -> m (Either e a))
  -> [Record]
  -> [Record]
  -> ApiItemsT [e] m [a]
mkUserPostsOnUpdate f existing = runAction mkRecord
  where
    mkRecord r = f (get r) r
    get r = Map.lookup (getIdValue' r) recordMap
    recordMap = mkIdIndexedMap existing

mkUserPostsOnCreate
  :: MonadIO m
  => [Record]
  -> [Record]
  -> [Record]
  -> ApiItemsT [ApiError] m [(Record, RecordId)]
mkUserPostsOnCreate subs posts = runAction mkRecord
  where
    mkRecord r = mkUserPostOnCreate (get subMap subId r) (get postMap postId r) r
    get m fid r = Map.lookup (fid r) m
    subMap = mkIdIndexedMap subs
    postMap = mkIdIndexedMap posts

mkUserPostOnCreate
  :: MonadIO m
  => Maybe Record
  -> Maybe Record
  -> Record
  -> m (Either ApiError (Record, RecordId))
mkUserPostOnCreate _ Nothing r = return . Left $ mk404Err postDefinition r
mkUserPostOnCreate Nothing _ r = return . Left $ mk404Err subscriptionDefinition r
mkUserPostOnCreate (Just sub) (Just post) input
  | not (postBelongsToSub sub post) =
    return . Left $ mk400Err "Post does not belong to subscription." input
  | otherwise = Right <$> mkUserPostTuple Nothing recId record
  where
    (recId, ids) = getIds sub post
    record = mergeRecords base input'
    sub' = includeFields ["title", "notes", "tags"] sub
    base = foldr (uncurry setValue) (mergeRecords sub' $ mkPost post) ids
    input' = excludeFields skipFieldsOnCreate input

mkUserPostOnUpdate
  :: MonadIO m
  => (Record -> Record -> Record)
  -> Maybe Record
  -> Record
  -> m (Either ApiError (Record, RecordId))
mkUserPostOnUpdate _ Nothing r = return . Left $ mk404Err userPostDefinition r
mkUserPostOnUpdate f (Just existing) input =
  Right <$>
  mkUserPostTuple
    (Just existing)
    (getIdValue' existing)
    (excludeFields [idLabel, "__v"] $ f existing record)
  where
    record = excludeFields skipFieldsOnUpdate input

mkUserPostOnReplace
  :: MonadIO m
  => Maybe Record -> Record -> m (Either ApiError (Record, RecordId))
mkUserPostOnReplace = mkUserPostOnUpdate (replaceRecords skipFieldsOnUpdate)

mkUserPostOnModify
  :: MonadIO m
  => Maybe Record -> Record -> m (Either ApiError (Record, RecordId))
mkUserPostOnModify = mkUserPostOnUpdate mergeRecords

mkUserPostTuple
  :: MonadIO m
  => Maybe Record -> RecordId -> Record -> m (Record, RecordId)
mkUserPostTuple existing recId record =
  flip (,) recId <$> addTimestamp existing record

postBelongsToSub :: Record -> Record -> Bool
postBelongsToSub sub post =
  getValue' "feedId" sub == (getValue' "feedId" post :: RecordId)

getUserPosts
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [RecordId] -> ExceptT ApiError m [Record]
getUserPosts ids =
  runEsAndExtract $ E.getByIds ids (recordCollection userPostDefinition)

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
addTimestamp existing new =
  case existingUTCDate of
    Nothing -> mergeDates new existingTextDate
    Just _ -> mergeDates new existingUTCDate
  where
    existingTextDate :: Maybe Text
    existingTextDate = existing >>= getValue createdAtLabel
    existingUTCDate :: Maybe UTCTime
    existingUTCDate = existing >>= getValue createdAtLabel
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
    lookup' name = fromJust . lookup name

-- ^
-- Generate an id for a user post
mkUserPostId' :: Record -> RecordId
mkUserPostId' record =
  mkUserPostId (getValue' "subscriptionId" record) (getValue' "postId" record)

-- ^
-- Generate an id given a subscription id and a post id
mkUserPostId :: RecordId -> RecordId -> RecordId
mkUserPostId subId' postId' = postId' <> "-" <> subId'

-- ^
-- Get the subscription id of a user-post
subId :: Record -> RecordId
subId = getValue' "subscriptionId"

-- ^
-- Get the post id of a user-post
postId :: Record -> RecordId
postId = getValue' "postId"

validateRecordTuple :: (Record, RecordId)
                    -> ((Record, RecordId), ValidationResult)
validateRecordTuple (r, rid) =
  first (flip (,) rid) $ validateRecord userPostDefinition r

skipFieldsOnUpdate :: [Label]
skipFieldsOnUpdate = ["post", "feedId", "userId", "postId", "subscriptionId"]

skipFieldsOnCreate :: [Label]
skipFieldsOnCreate =
  ["post", "userId", "feedId", createdAtLabel, updatedAtLabel, idLabel]
