{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Error utility functions
module Util.Error where

import Data.Bson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Network.HTTP.Types.Status
import Types.Common
import Util.Constants

-- ^
-- Create an API error
mkApiError :: Maybe Record -> Status -> String -> ApiError
mkApiError record status msg = ApiError record status (LBS.pack msg)

-- ^
-- Create a 400 error
mk400Err' :: String -> ApiError
mk400Err' = mkApiError Nothing status400

-- ^
-- Create a 500 error
mk500Err' :: String -> ApiError
mk500Err' = mkApiError Nothing status500

-- ^
-- Create a 400 error containing an input record
mk400Err :: String -> Record -> ApiError
mk400Err msg record = mkApiError (Just record) status400 msg

-- ^
-- Create a 404 error to be returned when an object is not found
mk404Err :: RecordDefinition -> Record -> ApiError
mk404Err def record =
  mkApiError (Just record) status404 $
  "Record not found in " ++
  T.unpack (recordCollectionName def) ++ " collection."

-- ^
-- Create a 404 error containing the id of a not found object
mk404IdErr :: RecordDefinition -> RecordId -> ApiError
mk404IdErr def rid = mk404Err def (Record [idLabel =: rid])
