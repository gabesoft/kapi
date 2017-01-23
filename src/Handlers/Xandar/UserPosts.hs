{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- ^
-- Handlers for the user posts endpoints
module Handlers.Xandar.UserPosts where

import Api.Xandar
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (encode)
import Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (intercalate)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Database.MongoDB (Pipe, Database, Index)
import Database.MongoDB.Query (Failure(..))
import Network.HTTP.Types.Status
import Persistence.Common
import Persistence.MongoDB
import Servant
import Servant.Utils.Enter (Enter)
import Types.Common

-- ^
-- Get multiple records
getMultiple :: ServerT GetMultiple Api
getMultiple = undefined

-- ^
-- Get a single record by id
getSingle :: Maybe Text -> Text -> Api (Headers '[Header "ETag" String] Record)
getSingle etag uid = undefined

-- ^
-- Delete a single record
deleteSingle :: Text -> Api NoContent
deleteSingle uid = undefined

-- ^
-- Create one or more records
createSingleOrMultiple
  :: ApiData Record
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingleOrMultiple (Single r) = undefined
createSingleOrMultiple (Multiple rs) = undefined

-- ^
-- Create a single record
createSingle
  :: Record
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingle input = undefined

-- ^
-- Create multiple records
createMultiple
  :: [Record]
  -> Api (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
createMultiple inputs = undefined

-- ^
-- Update (replace) a single record
replaceSingle :: Text -> Record -> Api Record
replaceSingle = undefined

-- ^
-- Update (modify) a single record
modifySingle :: Text -> Record -> Api Record
modifySingle = undefined

-- ^
-- Update (replace) multiple records
replaceMultiple :: [Record] -> Api [ApiResult]
replaceMultiple = undefined

-- ^
-- Update (modify) multiple records
modifyMultiple :: [Record] -> Api [ApiResult]
modifyMultiple = undefined

-- ^
-- Handle a head request for a single record endpoint
headSingle :: Text -> Api (Headers '[Header "ETag" String] NoContent)
headSingle uid = undefined

-- ^
-- Handle a head request for a multiple records endpoint
headMultiple :: Api (Headers '[Header "X-Total-Count" String] NoContent)
headMultiple = undefined

-- ^
-- Handle an options request for a single record endpoint
optionsSingle :: Text
              -> Api (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
optionsSingle _ = undefined

-- ^
-- Handle an options request for a multiple record endpoint
optionsMultiple :: Api (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
optionsMultiple = undefined
