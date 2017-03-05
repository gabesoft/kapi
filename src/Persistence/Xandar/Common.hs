{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Common functionality for persistence modules
module Persistence.Xandar.Common where

import Control.Monad.Except
import Control.Monad.Trans.Control
import qualified Data.Aeson as A
import Data.Bifunctor
import Data.Bson hiding (lookup, label)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid ((<>))
import Database.Bloodhound (EsError(..), SearchResult(..))
import Database.MongoDB
       (Database, Collection, Pipe, Failure, Index(..))
import Persistence.Common
import qualified Persistence.ElasticSearch as E
import Persistence.Facade (validateMulti)
import qualified Persistence.MongoDB as M
import Types.Common

subscriptionDefinition :: RecordDefinition
subscriptionDefinition =
  RecordDefinition "feedsubscriptions" "subscriptions" $
  Map.fromList
    [ mkIdDef "userId"
    , mkIdDef "feedId"
    , mkOptDef' "title"
    , mkOptDef' "notes"
    , mkOptDef "tags" (Just [] :: Maybe [String])
    , mkReqDef "disabled" (Just False :: Maybe Bool)
    , mkOptDef "unreadCount" (Just 0 :: Maybe Int)
    ]

subscriptionIndices :: [Index]
subscriptionIndices =
  [ Index
    { iColl = recordCollection subscriptionDefinition
    , iKey = ["userId" =: (1 :: Int), "feedId" =: (1 :: Int)]
    , iName = "userid_feedid_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  ]

userDefinition :: RecordDefinition
userDefinition =
  RecordDefinition "users" "users" $
  Map.fromList
    [ mkReqDef' "email"
    , mkReqDef "disabled" (Just False)
    , mkReqDef "admin" (Just False)
    , mkOptDef' "githubAvatar"
    , mkOptDef' "githubUrl"
    , mkOptDef' "githubLogin"
    ]

userIndices :: [Index]
userIndices =
  [ Index
    { iColl = recordCollection userDefinition
    , iKey = ["email" =: (1 :: Int)]
    , iName = "email_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  ]

feedDefinition :: RecordDefinition
feedDefinition =
  RecordDefinition "feeds" "feeds" $
  Map.fromList
    [ mkOptDef' "author"
    , mkOptDef' "data"
    , mkOptDef' "date"
    , mkOptDef' "description"
    , mkReqDef "failedAttempts" (Just 0 :: Maybe Int)
    , mkOptDef' "favicon"
    , mkOptDef' "format"
    , mkOptDef' "generator"
    , mkOptDef' "guid"
    , mkOptDef' "image"
    , mkOptDef' "language"
    , mkOptDef' "lastPostDate"
    , mkOptDef' "lastReadDate"
    , mkOptDef' "lastReadStatus"
    , mkOptDef' "link"
    , mkReqDef "postCount" (Just 0 :: Maybe Int)
    , mkOptDef' "title"
    , mkReqDef' "uri"
    ]

feedIndices :: [Index]
feedIndices =
  [ Index
    { iColl = recordCollection feedDefinition
    , iKey = ["uri" =: (1 :: Int)]
    , iName = "uri_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  ]

postDefinition :: RecordDefinition
postDefinition =
  RecordDefinition "posts" "posts" $
  Map.fromList
    [ mkOptDef' "author"
    , mkOptDef' "comments"
    , mkOptDef' "date"
    , mkOptDef' "description"
    , mkIdDef "feedId"
    , mkReqDef' "guid"
    , mkOptDef' "image"
    , mkOptDef' "inlineStatus"
    , mkReqDef' "link"
    , mkOptDef' "pubdate"
    , mkOptDef' "source"
    , mkOptDef' "summary"
    , mkOptDef' "title"
    ]

postIndices :: [Index]
postIndices =
  [ Index
    { iColl = recordCollection postDefinition
    , iKey = ["guid" =: (1 :: Int)]
    , iName = "guid_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  , Index
    { iColl = recordCollection postDefinition
    , iKey = ["link" =: (1 :: Int)]
    , iName = "link_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  ]

userPostDefinition :: RecordDefinition
userPostDefinition =
  RecordDefinition "post" "user-posts" $
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

postQueryDefinition :: RecordDefinition
postQueryDefinition =
  RecordDefinition "postqueries" "post-queries" $
  Map.fromList
    [ mkReqDef' "userId"
    , mkReqDef "pinState" (Just 0 :: Maybe Int)
    , mkOptDef' "title"
    , mkOptDef' "userText"
    , mkOptDef' "text"
    , mkOptDef' "query"
    , mkOptDef' "lastUsed"
    ]

postQueryIndices :: [Index]
postQueryIndices =
  [ Index
    { iColl = recordCollection postQueryDefinition
    , iKey = ["userId" =: (1 :: Int), "text" =: (1 :: Int)]
    , iName = "userid_text_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  ]

subscriptionCollection :: Collection
subscriptionCollection = recordCollection subscriptionDefinition

userCollection :: Collection
userCollection = recordCollection userDefinition

feedCollection :: Collection
feedCollection = recordCollection feedDefinition

postCollection :: Collection
postCollection = recordCollection postDefinition

userPostCollection :: Collection
userPostCollection = recordCollection userPostDefinition

postQueryCollection :: Collection
postQueryCollection = recordCollection postQueryDefinition

validateSubscriptions
  :: Monad m
  => [Record] -> ApiItemsT [ApiError] m [Record]
validateSubscriptions = validateMulti subscriptionDefinition

validateUsers
  :: Monad m
  => [Record] -> ApiItemsT [ApiError] m [Record]
validateUsers = validateMulti userDefinition

validatePosts
  :: Monad m
  => [Record] -> ApiItemsT [ApiError] m [Record]
validatePosts = validateMulti postDefinition

validateFeeds
  :: Monad m
  => [Record] -> ApiItemsT [ApiError] m [Record]
validateFeeds = validateMulti feedDefinition

validateUserPosts
  :: Monad m
  => [Record] -> ApiItemsT [ApiError] m [Record]
validateUserPosts = validateMulti userPostDefinition

validatePostQueries
  :: Monad m
  => [Record] -> ApiItemsT [ApiError] m [Record]
validatePostQueries = validateMulti postQueryDefinition