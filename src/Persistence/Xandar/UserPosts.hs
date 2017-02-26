{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Persistence functionality for user-posts
module Persistence.Xandar.UserPosts
  ( createUserPost
  , createUserPosts
  , esInsert
  , esInsertMulti
  , esModify
  , esModifyMulti
  , esReplace
  , esReplaceMulti
  , indexUserPosts
  , indexUserPosts'
  , mkUserPostId
  , mkUserPostId'
  , modifyUserPost
  , modifyUserPosts
  , replaceUserPost
  , replaceUserPosts
  ) where

import Control.Applicative ((<|>))
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
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Bloodhound (EsError(..), SearchResult(..))
import Database.MongoDB (Database, Pipe, Failure)
import Debug.Trace
import Network.HTTP.Types
import Persistence.Common
import qualified Persistence.ElasticSearch as E
import Persistence.Facade
import qualified Persistence.MongoDB as M
import Persistence.Xandar.Common
import Types.Common
import Util.Constants
import Util.Error

esInsert
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => Record -> ExceptT ApiError m Record
esInsert = toSingle esInsertMulti

esReplace
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => Record -> ExceptT ApiError m Record
esReplace = toSingle esReplaceMulti

esModify
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => Record -> ExceptT ApiError m Record
esModify = toSingle esModifyMulti

esReplaceMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [Record] -> ApiItems2T [ApiError] m [Record]
esReplaceMulti = esUpdateMulti replaceUserPosts

esModifyMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [Record] -> ApiItems2T [ApiError] m [Record]
esModifyMulti = esUpdateMulti modifyUserPosts

esInsertMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => [Record] -> ApiItems2T [ApiError] m [Record]
esInsertMulti input = do
  valid <- validateUserPosts input
  subs <- getExistingMulti subscriptionDefinition (subId <$> valid)
  posts <- getExistingMulti postDefinition (postId <$> valid)
  records <- createUserPosts subs posts valid
  toMulti (indexUserPosts records)

esUpdateMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => ([Record] -> [Record] -> ApiItems2T [ApiError] m [(Record, RecordId)])
  -> [Record]
  -> ApiItems2T [ApiError] m [Record]
esUpdateMulti update input = do
  valid1 <- validateEsIdMulti input
  existing <- toMulti (getUserPosts $ getIdValue' <$> valid1)
  records <- update existing valid1
  valid2 <- validateMulti' fst validateRecordTuple records
  toMulti (indexUserPosts valid2)

-- ^
-- Index multiple documents and re-query them
indexUserPosts
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => [(Record, RecordId)] -> ExceptT ApiError m [Record]
indexUserPosts input =
  indexUserPosts' input >>
  runEsAndExtract (E.getByIds (snd <$> input) userPostCollection)

-- ^
-- Index multiple documents
indexUserPosts'
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => [(Record, RecordId)] -> ExceptT ApiError m Text
indexUserPosts' input =
  runEs (E.indexDocuments input userPostCollection) <* runEs E.refreshIndex

replaceUserPosts
  :: (MonadIO m)
  => [Record] -> [Record] -> ApiItems2T [ApiError] m [(Record, RecordId)]
replaceUserPosts = updateUserPosts replaceUserPost

modifyUserPosts
  :: (MonadIO m)
  => [Record] -> [Record] -> ApiItems2T [ApiError] m [(Record, RecordId)]
modifyUserPosts = updateUserPosts modifyUserPost

updateUserPosts
  :: Monad m
  => (Maybe Record -> Record -> m (Either e a))
  -> [Record]
  -> [Record]
  -> ApiItems2T [e] m [a]
updateUserPosts f existing = runAction mkRecord
  where
    mkRecord r = f (get r) r
    get r = Map.lookup (getIdValue' r) recordMap
    recordMap = mkIdIndexedMap existing

createUserPosts
  :: MonadIO m
  => [Record]
  -> [Record]
  -> [Record]
  -> ApiItems2T [ApiError] m [(Record, RecordId)]
createUserPosts subs posts = runAction mkRecord
  where
    mkRecord r = createUserPost (get subMap subId r) (get postMap postId r) r
    get m fid r = Map.lookup (fid r) m
    subMap = mkIdIndexedMap subs
    postMap = mkIdIndexedMap posts

createUserPost
  :: MonadIO m
  => Maybe Record
  -> Maybe Record
  -> Record
  -> m (Either ApiError (Record, RecordId))
createUserPost _ Nothing r = return . Left $ mk404Err postDefinition r
createUserPost Nothing _ r = return . Left $ mk404Err subscriptionDefinition r
createUserPost (Just sub) (Just post) input
  | not (postBelongsToSub sub post) =
    return . Left $ mk400Err "Post does not belong to subscription." input
  | otherwise = Right <$> mkUserPostTuple Nothing recId record
  where
    (recId, ids) = getIds sub post
    record = mergeRecords base input'
    sub' = includeFields ["title", "notes", "tags"] sub
    base = foldr (uncurry setValue) (mergeRecords sub' $ mkPost post) ids
    input' =
      excludeFields
        ["post", "userId", "feedId", createdAtLabel, updatedAtLabel, idLabel]
        input

replaceUserPost
  :: MonadIO m
  => Maybe Record -> Record -> m (Either ApiError (Record, RecordId))
replaceUserPost = updateUserPost (replaceRecords keepFieldsOnUpdate)

modifyUserPost
  :: MonadIO m
  => Maybe Record -> Record -> m (Either ApiError (Record, RecordId))
modifyUserPost = updateUserPost mergeRecords

updateUserPost
  :: MonadIO m
  => (Record -> Record -> Record)
  -> Maybe Record
  -> Record
  -> m (Either ApiError (Record, RecordId))
updateUserPost _ Nothing r = return . Left $ mk404Err userPostDefinition r
updateUserPost f (Just existing) input =
  Right <$>
  mkUserPostTuple
    (Just existing)
    (getIdValue' existing)
    (excludeFields [idLabel] $ f existing record)
  where
    record = excludeFields keepFieldsOnUpdate input

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
mkUserPostId subId' postId' = subId' <> "-" <> postId'

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

keepFieldsOnUpdate :: [Text]
keepFieldsOnUpdate = ["feedId", "userId", "postId", "subscriptionId"]
