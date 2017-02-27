{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Error utility functions
module Util.Error
  ( mk400Err
  , mk400Err'
  , mk404Err
  , mk404IdErr
  , mk500Err'
  , throwApiError
  ) where

import Control.Monad.Except
import Data.Aeson (encode)
import Data.Bson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Network.HTTP.Types.Status
import Servant
import Types.Common
import Util.Constants

-- ^
-- Create a 400 error
mk400Err' :: String -> ApiError
mk400Err' = mkApiErr Nothing status400

-- ^
-- Create a 500 error
mk500Err' :: String -> ApiError
mk500Err' = mkApiErr Nothing status500

-- ^
-- Create a 400 error containing an input record
mk400Err :: String -> Record -> ApiError
mk400Err msg record = mkApiErr (Just record) status400 msg

-- ^
-- Create a 404 error to be returned when an object is not found
mk404Err :: RecordDefinition -> Record -> ApiError
mk404Err def record =
  mkApiErr (Just record) status404 $
  "Record not found in " ++
  T.unpack (recordCollectionName def) ++ " collection."

-- ^
-- Create a 404 error containing the id of a not found object
mk404IdErr :: RecordDefinition -> RecordId -> ApiError
mk404IdErr def rid = mk404Err def (Record [idLabel =: rid])

-- ^
-- Convert an 'ApiError' into a 'ServantErr' and throw
throwApiError
  :: MonadError ServantErr m
  => ApiError -> m a
throwApiError err = throwError (mkServantErr err)

-- ^
-- Convert an 'ApiError' into a 'ServantErr'
mkServantErr :: ApiError -> ServantErr
mkServantErr err =
  ServantErr
  { errHTTPCode = statusCode status
  , errReasonPhrase = BS.unpack (statusMessage status)
  , errBody = body
  , errHeaders = []
  }
  where
    status = apiErrorStatus err
    msg = apiErrorMessage err
    body
      | LBS.null msg = mempty
      | otherwise = encode (Fail err :: ApiItem ApiError ())

-- ^
-- Create an API error
mkApiErr :: Maybe Record -> Status -> String -> ApiError
mkApiErr record status msg = ApiError record status (LBS.pack msg)
