{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Common types
module Types.Common where

import Control.Lens (view, over)
import Data.Aeson as AESON
import Data.AesonBson (aesonify, bsonify)
import Data.Bson as BSON
import Data.List (find)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import GHC.Generics

-- |
-- Type for id fields
type RecordId = Text

-- |
-- Representation for a data item
data Record =
  Record [Field]
  deriving (Eq, Show)

instance ToJSON Record where
  toJSON (Record r) = Object (aesonify r)

instance FromJSON Record where
  parseJSON (Object obj) = return $ Record (bsonify obj)
  parseJSON _ = fail "empty"

-- |
-- Lens for record objects
-- Non-existent fields will yield @Null@ on @view@ and will
-- cause a field to be added on @over@
recLens
  :: Functor f
  => Label -> (Field -> f Field) -> Record -> f Record
recLens l f r =
  (\nf -> setField r <$> nf) (f $ fromMaybe (l =: BSON.Null) (getField r l))

-- |
-- Lens used for deleting fields from a record
delLens
  :: Functor f
  => Label -> (Field -> f Field) -> Record -> f Record
delLens l f r = (\nf -> const (delField r l) <$> nf) (f (l =: BSON.Null))

-- |
-- Get the value of a field in a record
-- For non-existent fields a value of @Null@ will be returned
(^=.)
  :: Val a
  => Record -> Label -> Maybe a
(^=.) r l = BSON.cast (value $ view (recLens l) r)

-- |
-- Set the value of a field in a record
-- Existing fields are overwritten and non-existent ones are added
(.=~)
  :: Val v
  => Label -> v -> Record -> Record
(.=~) l v = over (recLens l) (const (l =: v))

-- |
-- Delete the value of a field in a record
(./~) :: Record -> Label -> Record
(./~) r l = over (delLens l) (const (l =: BSON.Null)) r

-- |
-- Set a record field
setField :: Record -> Field -> Record
setField r x = modField (label x) r (const $ Just x) (Just x)

-- |
-- Delete a record field
delField :: Record -> Label -> Record
delField r l = modField l r (const Nothing) Nothing

-- |
-- Modify a record field
modField :: Label -> Record -> (Field -> Maybe Field) -> Maybe Field -> Record
modField l (Record xs) f empty = Record (mod xs)
  where
    mod [] = maybeToList empty
    mod (a:as)
      | label a == l = maybeToList (f a) ++ as
      | otherwise = a : mod as

-- |
-- Get a record field
getField :: Record -> Label -> Maybe Field
getField (Record xs) l = find ((== l) . label) xs

-- |
-- Sample records. TO BE REMOVED.
-- "_id" =: (read "584e58195984185eb8000005" :: ObjectId)
u1 =
  Record
    [ "_id" =: ("584e58195984185eb8000005" :: String)
    , "email" =: ("blue@leaf.com" :: String)
    , "githubUrl" =: ("https://github.com/api/users/mrblue" :: String)
    ]

u2 =
  Record
    [ "_id" =: ("584e58195984185eb8000006" :: String)
    , "email" =: ("green@leaf.com" :: String)
    , "githubUrl" =: ("https://github.com/api/users/mrgreen" :: String)
    ]

-- |
-- Representation for an API error
data ApiError = ApiError
  { _message :: String
  } deriving (Eq, Show, Generic)

instance ToJSON ApiError

-- |
-- Representation for an API item result
-- An item result could be an error or a record
data ApiItem a
  = Fail ApiError
  | Succ a
  deriving (Eq, Show)

instance (ToJSON a) =>
         ToJSON (ApiItem a) where
  toJSON (Fail e) = object ["error" .= toJSON e]
  toJSON (Succ a) = toJSON a
