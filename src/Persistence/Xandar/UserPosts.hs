{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Persistence functionality for user-posts
module Persistence.Xandar.UserPosts
  ( esInsert
  , esInsertMulti
  , esModify
  , esModifyMulti
  , esReplace
  , esReplaceMulti
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
import qualified Data.Text as T
import Database.Bloodhound (EsError(..), SearchResult(..))
import Database.MongoDB (Database, Pipe, Failure)
import Debug.Trace
import Network.HTTP.Types
import Persistence.Common
import qualified Persistence.ElasticSearch as E
import Persistence.Facade
       (validate, validateId, validateMulti, validateEsIdMulti,
        getExistingMulti, runEs, runEsAndExtract, toSingle, toMulti)
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
  valid <- validateMulti userPostDefinition input
  subs <- getExistingMulti subscriptionDefinition (subId <$> valid)
  posts <- getExistingMulti postDefinition (postId <$> valid)
  records <- createUserPosts (subs, posts) valid
  toMulti (indexDocuments records)

esUpdateMulti
  :: (MonadIO m, MonadReader ApiConfig m, MonadBaseControl IO m)
  => ([Record] -> [Record] -> ApiItems2T [ApiError] m [(Record, RecordId)])
  -> [Record]
  -> ApiItems2T [ApiError] m [Record]
esUpdateMulti update input = do
  valid1 <- validateEsIdMulti input
  existing <- toMulti (getUserPosts $ getIdValue' <$> valid1)
  records <- update existing valid1
  valid2 <- validateMulti' fst validateUserPostTuple records
  toMulti (indexDocuments valid2)

indexDocuments
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => [(Record, RecordId)] -> ExceptT ApiError m [Record]
indexDocuments input = do
  let mapping = recordCollection userPostDefinition
  _ <- runEs (E.indexDocuments input mapping)
  _ <- runEs E.refreshIndex
  runEsAndExtract (E.getByIds (snd <$> input) mapping)

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

replaceUserPosts
  :: (MonadIO m)
  => [Record] -> [Record] -> ApiItems2T [ApiError] m [(Record, RecordId)]
replaceUserPosts = updateUserPosts replaceUserPost

modifyUserPosts
  :: (MonadIO m)
  => [Record] -> [Record] -> ApiItems2T [ApiError] m [(Record, RecordId)]
modifyUserPosts = updateUserPosts modifyUserPost

updateUserPosts f existing input = do
  records <- mapM mkRecord input
  ApiItems2T . return $ eitherToItems records
  where
    mkRecord r = f (get r) r
    get r = Map.lookup (getIdValue' r) recordMap
    recordMap = mkIdIndexedMap existing

createUserPosts
  :: (MonadIO m)
  => ([Record], [Record])
  -> [Record]
  -> ApiItems2T [ApiError] m [(Record, RecordId)]
createUserPosts (subs, posts) input = do
  records <- mapM mkRecord input
  ApiItems2T . return $ eitherToItems records
  where
    mkRecord r = createUserPost (get subMap subId r, get postMap postId r) r
    get m fid r = Map.lookup (fid r) m
    subMap = mkIdIndexedMap subs
    postMap = mkIdIndexedMap posts

createUserPost
  :: (MonadIO m)
  => (Maybe Record, Maybe Record)
  -> Record
  -> m (Either ApiError (Record, RecordId))
createUserPost (_, Nothing) r = return . Left $ mk404Err postDefinition r
createUserPost (Nothing, _) r =
  return . Left $ mk404Err subscriptionDefinition r
createUserPost (Just sub, Just post) input
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
replaceUserPost Nothing r = return . Left $ mk404Err userPostDefinition r
replaceUserPost (Just existing) input =
  Right <$>
  mkUserPostTuple
    (Just existing)
    (getIdValue' existing)
    (excludeFields [idLabel] $ replaceRecords [createdAtLabel] existing input)

modifyUserPost
  :: MonadIO m
  => Maybe Record -> Record -> m (Either ApiError (Record, RecordId))
modifyUserPost Nothing r = return . Left $ mk404Err userPostDefinition r
modifyUserPost (Just existing) input =
  Right <$>
  mkUserPostTuple
    (Just existing)
    (getIdValue' existing)
    (excludeFields [idLabel] $ mergeRecords existing input)

mkUserPostTuple
  :: MonadIO m
  => Maybe Record -> RecordId -> Record -> m (Record, RecordId)
mkUserPostTuple existing recId record =
  flip (,) recId <$> addTimestamp existing record

postBelongsToSub :: Record -> Record -> Bool
postBelongsToSub sub post =
  getValue' "feedId" sub == (getValue' "feedId" post :: RecordId)

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
mkUserPost _ (_, Nothing) (_, r) = return . Left $ mk404Err postDefinition r
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

-- ^
-- Make a 404 error to be returned when attempting to construct an invalid user post
-- TODO: consolidate error message with same from 'getExistingMulti'
-- TODO: consolidate with other errors
mk404Err :: RecordDefinition -> Record -> ApiError
mk404Err def record =
  ApiError (Just record) status400 $
  LBS.pack $
  "Record not found in " ++
  T.unpack (recordCollectionName def) ++ " collection."

validateUserPostTuple :: (Record, RecordId)
                      -> ((Record, RecordId), ValidationResult)
validateUserPostTuple (r, rid) =
  first (flip (,) rid) $ validateRecord userPostDefinition r

-- TODO: consolidate with Facade
validateMulti'
  :: (Monad m)
  => (a -> Record)
  -> (a -> (a, ValidationResult))
  -> [a]
  -> ApiItems2T [ApiError] m [a]
validateMulti' f v records =
  ApiItems2T . return . concatItems $ (vResultToItems f . v) <$> records

vResultToItems :: (a -> Record)
               -> (a, ValidationResult)
               -> ApiItems2 [ApiError] [a]
vResultToItems _ (a, ValidationErrors []) = ApiItems2 [] [a]
vResultToItems f (a, err) =
  ApiItems2 [ApiError (Just $ f a) status400 (A.encode err)] []
