{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Common functionality for RSS readers persistence modules
module Persistence.RssReaders.Common where

import Data.Bson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Database.MongoDB (Collection, Index(..))
import Persistence.Common
import Persistence.Facade (validateMulti)
import Types.Common

subscriptionDefinition :: RecordDefinition
subscriptionDefinition =
  RecordDefinition "feedsubscriptions" "subscriptions" subscriptionIndices $
  Map.fromList
    [ mkIdDef "userId"
    , mkIdDef "feedId"
    , mkOptDef' "title"
    , mkOptDef' "notes"
    , mkOptDef "tags" ([] :: [String])
    , mkReqDef "disabled" False
    , mkOptDef "unreadCount" (0 :: Int)
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

feedDefinition :: RecordDefinition
feedDefinition =
  RecordDefinition "feeds" "feeds" feedIndices $
  Map.fromList
    [ mkOptDef' "author"
    , mkOptDef' "data"
    , mkDateDef' "date"
    , mkOptDef' "description"
    , mkReqDef "failedAttempts" (0 :: Int)
    , mkOptDef' "favicon"
    , mkOptDef' "format"
    , mkOptDef' "generator"
    , mkOptDef' "guid"
    , mkOptDef' "image"
    , mkOptDef' "language"
    , mkDateDef' "lastPostDate"
    , mkDateDef' "lastReadDate"
    , mkOptDef' "lastReadStatus"
    , mkOptDef' "link"
    , mkReqDef "postCount" (0 :: Int)
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
  RecordDefinition "posts" "posts" postIndices $
  Map.fromList
    [ mkOptDef' "author"
    , mkOptDef' "comments"
    , mkDateDef' "date"
    , mkOptDef' "description"
    , mkIdDef "feedId"
    , mkReqDef' "guid"
    , mkOptDef' "image"
    , mkOptDef' "inlineStatus"
    , mkReqDef' "link"
    , mkDateDef' "pubdate"
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
  RecordDefinition "post" "user-posts" [] $
  Map.fromList
    [ mkIdDef "subscriptionId"
    , mkIdDef "postId"
    , mkOptDef' "feedId"
    , mkOptDef' "userId"
    , mkReqDef "read" False
    , mkOptDef' "post"
    , mkOptDef' "title"
    , mkOptDef "tags" ([] :: [String])
    ]

postQueryDefinition :: RecordDefinition
postQueryDefinition =
  RecordDefinition "postqueries" "post-queries" postQueryIndices $
  Map.fromList
    [ mkIdDef "userId"
    , mkReqDef "pinState" (0 :: Int)
    , mkReqDef "isSearch" False
    , mkOptDef' "title"
    , mkOptDef' "userText"
    , mkOptDef' "text"
    , mkOptDef' "hash"
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

tagsDefinition :: RecordDefinition
tagsDefinition =
  RecordDefinition "tags" "tags" tagsIndices $
  Map.fromList [mkIdDef "userId", mkOptDef "tags" ([] :: [Text])]

tagsIndices :: [Index]
tagsIndices =
  [ Index
    { iColl = recordCollection tagsDefinition
    , iKey = ["userId" =: (1 :: Int)]
    , iName = "userid_unique"
    , iUnique = True
    , iDropDups = True
    , iExpireAfterSeconds = Nothing
    }
  ]

subscriptionCollection :: Collection
subscriptionCollection = recordCollection subscriptionDefinition

feedCollection :: Collection
feedCollection = recordCollection feedDefinition

postCollection :: Collection
postCollection = recordCollection postDefinition

userPostCollection :: Collection
userPostCollection = recordCollection userPostDefinition

postQueryCollection :: Collection
postQueryCollection = recordCollection postQueryDefinition

tagsCollection :: Collection
tagsCollection = recordCollection tagsDefinition

validateSubscriptions
  :: Monad m
  => [Record] -> ApiItemsT [ApiError] m [Record]
validateSubscriptions = validateMulti subscriptionDefinition

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
