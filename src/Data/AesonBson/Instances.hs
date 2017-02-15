{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- ^
-- Original location https://github.com/nh2/aesonbson/blob/master/Data/AesonBson/Instances.hs

-- ^ Provides @ToJSON@ instances for BSON @Value@s and @Document@s.
module Data.AesonBson.Instances where

import Data.Aeson.Types as AESON
import Data.Bson as BSON

import Data.AesonBson

instance ToJSON BSON.Value where
  toJSON = aesonifyValue
