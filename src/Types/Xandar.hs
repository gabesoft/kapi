{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Models for xandar
module Types.Xandar where

import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Text (Text)
import GHC.Generics

type UserId = Text

data Error = Error
    { _message :: String
    } deriving (Eq,Show,Generic)

instance ToJSON Error

data User = User
    { _userId :: Maybe UserId
    , _userEmail :: Text
    , _userIsAdmin :: Maybe Bool
    , _userPassword :: Maybe Text
    } deriving (Eq,Show,Generic)

data Feed = Feed
    { _feedAuthor :: Maybe Text
    , _feedDescription :: Maybe Text
    , _feedPostCount :: Int
    , _feedTitle :: Text
    , _feedUri :: Text
    } deriving (Eq,Show,Generic)

instance ToJSON Feed

deriveJSON
    defaultOptions
    { fieldLabelModifier = map toLower . drop 5
    , omitNothingFields = True
    }
    ''User

data ModelOrError a
    = Fail Error -- ^ Error case
    | Succ a     -- ^ Success case containing a value

instance (ToJSON a) =>
         ToJSON (ModelOrError a) where
    toJSON (Fail e) = toJSON e
    toJSON (Succ a) = toJSON a

u1 = User (Just "123") "milky@way.com" (Just True) Nothing

u2 = User (Just "998") "super@nova.com" (Just False) (Just "top-secret")