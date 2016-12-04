{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Models for xandar
module Types.Xandar where

import Data.Text (Text)
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import GHC.Generics
import Test.QuickCheck
import Data.Text.Arbitrary

  -- TODO: move arbitrary instances to spec and remove quickcheck imports

type UserId = Text

data User = User
  { _userId :: UserId
  , _userIsAdmin :: Bool
  , _userEmail :: Text
  , _userPassword :: Maybe Text
  } deriving (Eq, Show, Generic)

instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

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

u1 = User "123" True "milky@way.com" Nothing

u2 = User "998" False "super@nova.com" (Just "abcd")
