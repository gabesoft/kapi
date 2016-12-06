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
    { _userId :: UserId
    , _userIsAdmin :: Bool
    , _userEmail :: Text
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
    = Fail Error
    | Succ a

instance (ToJSON a) =>
         ToJSON (ModelOrError a) where
    toJSON (Fail e) = toJSON e
    toJSON (Succ a) = toJSON a

u1 = User "123" True "milky@way.com" Nothing

u2 = User "998" False "super@nova.com" (Just "top-secret")