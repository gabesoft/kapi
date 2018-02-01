{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Endpoints documentation
module Api.Documentation (markdownDocs) where

import Api.Main
import Data.Bson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Persistence.Common (setIdValue)
import Servant
import Servant.Docs
import Types.Common

recordId :: Text
recordId = "5a82994d5984184d70000003"

samplePost :: Record
samplePost = setIdValue recordId samplePostWithoutId

samplePostWithoutId :: RecordData Field
samplePostWithoutId =
  Record
    [ "author" =: ("Blue Penner" :: Text)
    , "date" =: ("2016-03-01T18:30:00.000Z" :: Text)
    , "description" =: ("Post content goes here" :: Text)
    , "feedId" =: ("56d7d88bc788cb1d6eb9199c" :: Text)
    , "guid" =: ("2175abce-0708-11e8-a088-1c1b0d977259" :: Text)
    , "summary" =: ("Brief post summary" :: Text)
    , "title" =: ("The impact of AI on dreams" :: Text)
    ]

sampleFeed :: Record
sampleFeed = setIdValue recordId sampleFeedWithoutId

sampleFeedWithoutId :: RecordData Field
sampleFeedWithoutId =
  Record
    [ "_id" =: ("5a72994d5984184d70000003" :: Text)
    , "author" =: ("Prolific Blogger" :: Text)
    , "date" =: ("2016-03-01T18:30:00.000Z" :: Text)
    , "description" =: ("Random thoughts about everything" :: Text)
    , "title" =: ("The art of randomness" :: Text)
    , "guid" =: ("2175abce-0708-11e8-a088-1c1b0d977259" :: Text)
    , "link" =: ("http://my-random-thoughts.com" :: Text)
    , "uri" =: ("http://my-random-thoughts.com/feed" :: Text)
    ]

sampleUser :: Record
sampleUser = setIdValue recordId sampleUserWithoutId

sampleUserWithoutId :: RecordData Field
sampleUserWithoutId =
  Record
    [ "_id" =: ("5a82994a5984184d70000003" :: Text)
    , "email" =: ("user@email.com" :: Text)
    , "admin" =: False
    , "disabled" =: False
    , "name" =: ("User One" :: Text)
    , "imageUrl" =: ("https://cdn.com/users/images/user-one.png" :: Text)
    ]

sampleTags :: Record
sampleTags = setIdValue recordId sampleTagsWithoutId

sampleTagsWithoutId :: RecordData Field
sampleTagsWithoutId =
  Record
    [ "_id" =: ("5a88994a5984184d70000003" :: Text)
    , "userId" =: ("54c31312bef9f6ce5fc1f23d" :: Text)
    , "tags" =: (["web-design", "sql", "mongodb"] :: [Text])
    ]

sampleQuery :: Record
sampleQuery = setIdValue recordId sampleQueryWithoutId

sampleQueryWithoutId :: RecordData Field
sampleQueryWithoutId =
  Record
    [ "userText" =: ("\"monad cofunctor\" & :unread" :: Text)
    , "createdAt" =: ("2018-02-01T05:23:21.353Z" :: Text)
    , "text" =: ("(\"monad cofunctor\" & :unread)" :: Text)
    , "pinState" =: (0 :: Int)
    , "userId" =: ("56d7cc3fccee17506699e735" :: Text)
    , "isSearch" =: False
    , "query" =:
      ("(read eq false) and (post.title contains 'monad cofunctor' or post.description:2 contains 'monad cofunctor')" :: Text)
    , "lastUsed" =: ("2017-03-05T03:32:24.903Z" :: Text)
    , "updatedAt" =: ("2018-02-01T05:23:21.353Z" :: Text)
    , "title" =: ("Posts containing monad topics" :: Text)
    ]

instance ToParam (QueryParams "include" Text) where
  toParam _ =
    DocQueryParam
      "include"
      ["name", "profile.description"]
      "a list of fields to be included in the objects returned. Nested names are allowed."
      List

instance ToParam (QueryParam "where" Text) where
  toParam _ =
    DocQueryParam
      "where"
      ["(name eq 'Sherlock') or (profile.date lt '12999888')"]
      "record filters"
      Normal

instance ToParam (QueryParams "sort" Text) where
  toParam _ =
    DocQueryParam
      "sort"
      ["+name", "-createdAt"]
      "sort fields +ascending or -descending"
      List

instance ToParam (QueryParam "page" Int) where
  toParam _ =
    DocQueryParam "page" ["1"] "the current page number for collection results" Normal

instance ToParam (QueryParam "per_page" Int) where
  toParam _ =
    DocQueryParam "per_page" ["50"] "the number of records per page" Normal

instance ToSample (ApiItem ApiError Record) where
  toSamples _ =
    [ ("Feed", Succ sampleFeedWithoutId)
    , ("Post", Succ samplePostWithoutId)
    , ("Tags", Succ sampleTagsWithoutId)
    , ("User", Succ sampleUserWithoutId)
    , ("Query", Succ sampleQueryWithoutId)
    ]

instance ToSample (ApiData ApiResult) where
  toSamples _ =
    [ ("Single post", Single $ Succ samplePost)
    , ("Single feed", Single $ Succ sampleFeed)
    , ("Multiple posts", Multiple [Succ samplePost, Succ samplePost])
    , ("Multiple feeds", Multiple [Succ sampleFeed, Succ sampleFeed])
    , ("Tags", Single $ Succ sampleTags)
    , ("Query", Single $ Succ sampleQuery)
    ]

instance ToSample (ApiData Record) where
  toSamples _ =
    [ ("Single post", Single samplePost)
    , ("Single feed", Single sampleFeed)
    , ("Multiple posts", Multiple [samplePost, samplePost])
    , ("Multiple feeds", Multiple [sampleFeed, sampleFeed])
    , ("Tags", Single sampleTags)
    , ("Query", Single sampleQuery)
    ]

instance ToSample Record where
  toSamples _ =
    [ ("User", sampleUser)
    , ("Post", samplePost)
    , ("Feed", sampleFeed)
    , ("Tags", sampleTags)
    , ("Query", sampleQuery)
    ]

instance ToSample Char where
  toSamples _ =
    [("a", 'a'), ("b", 'b'), ("c", 'c'), ("d", 'd'), ("e", 'e'), ("z", 'z')]

instance ToCapture (Capture "id" Text) where
  toCapture _ = DocCapture "_id" "record identifier"

intro :: DocIntro
intro = DocIntro "Welcome" ["This is the Kapi API Documentation"]

markdownDocs :: ByteString
markdownDocs =
  encodeUtf8 . pack . markdown $ docsWithIntros [intro] mainApiProxy
