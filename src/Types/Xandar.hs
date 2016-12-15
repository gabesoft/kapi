{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Models for xandar
module Types.Xandar where

import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Text (Text)
import Data.Time
import GHC.Generics

type UserId = Text

data User = User
  { _userId :: Maybe UserId
  , _userEmail :: Text
  , _userIsAdmin :: Maybe Bool
  , _userIsDisabled :: Maybe Bool
  , _userCreatedAt :: Maybe UTCTime
  , _userUpdatedAt :: Maybe UTCTime
  , _githubAvatar :: Maybe Text
  , _githubUrl :: Maybe Text
  , _githubLogin :: Maybe Text
  } deriving (Eq, Show, Generic)

data Feed = Feed
  { _feedAuthor :: Maybe Text
  , _feedDescription :: Maybe Text
  , _feedPostCount :: Int
  , _feedTitle :: Text
  , _feedUri :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Feed

deriveJSON
  defaultOptions
  {fieldLabelModifier = map toLower . drop 5, omitNothingFields = True}
  ''User
