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
import Data.Either
import Data.List (intercalate)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Database.Bloodhound (SearchResult(..))
import qualified Database.Bloodhound as B
import Network.HTTP.Types.Status
import Parsers.Filter (parse)
import Persistence.Common
import Persistence.ElasticSearch
import Servant
import Servant.Utils.Enter (Enter)
import Types.Common

mappingName :: Text
mappingName = "post"

esIndex :: ApiConfig -> Text
esIndex = confGetEsIndex appName

-- ^
-- Get multiple records
getMultiple :: ServerT GetMultiple Api
getMultiple include query sort page perPage = do
  conf <- ask
  let server = esServer conf
  let index = esIndex conf
  case prepareSearch query sort include 0 0 of
    Left err -> throwApiError $ ApiError (LBS.pack err) status400
    Right countSearch -> do
      count <- liftIO $ countDocuments server index mappingName countSearch
      let pagination =
            paginate (fromMaybe 1 page) (fromMaybe perPageDefault perPage) count
      let start = paginationStart pagination
      let limit = paginationLimit pagination
      -- TODO don't include the `include fields` in search and clean up results using `includeFields`
      let search = fromRight $ prepareSearch query sort include start limit
      results <- liftIO $ searchDocuments server index mappingName search
      -- TODO use the error status below
      case results of
        Left err ->
          throwApiError $ ApiError (LBS.pack . T.unpack $ B.errorMessage err) status500
        Right res -> do
          let (records, total) = extractRecords res
          return $
            addHeader (show $ paginationPage pagination) $
            addHeader (show $ paginationTotal pagination) $
            addHeader (show $ paginationLast pagination) $
            addHeader (show $ paginationSize pagination) $
            addHeader (intercalate "," $ mkPaginationLinks pagination) records
  where
    getLink = mkUserPostGetMultipleLink
    mkUrl page' = getLink include query sort (Just page') perPage
    mkPaginationLinks pagination =
      uncurry mkRelLink . second mkUrl <$>
      [ ("next", paginationNext pagination)
      , ("last", paginationLast pagination)
      , ("first", paginationFirst pagination)
      , ("prev", paginationPrev pagination)
      ]

-- ^
-- Create a link element
mkLink :: String -> String
mkLink link = "<" ++ link ++ ">"

-- ^
-- Create a relative link element
-- TODO consolidate with mkRelLink from Handlers.Common
--      also mkPaginationLinks, mkLink, and mkUrl
mkRelLink :: String -> String -> String
mkRelLink rel link = mkLink link ++ "; rel=\"" ++ rel ++ "\""

fromRight (Right x) = x
fromRight (Left _) = error "right expected"

-- prepareSearch :: Maybe Text
prepareSearch query sort fields start limit = toSearch <$> expr
  where
    expr = sequence $ first ("Invalid query: " ++) . parse <$> query
    toSearch expr = mkSearch expr sort fields start limit

-- ^
-- Convert an 'ApiError' into a 'ServantErr' and throw
-- TODO consolidate
throwApiError
  :: MonadError ServantErr m
  => ApiError -> m a
throwApiError err
  | apiErrorStatus err == status400 = throwError (mkErr err400 err)
  | apiErrorStatus err == status404 = throwError (mkErr err404 err)
  | otherwise = throwError (mkErr err500 err)
  where
    mkErr sErr aErr =
      sErr
      { errBody = encode (Fail aErr :: ApiItem ApiError ())
      }

-- ^
-- Get a single record by id
getSingle :: Maybe Text -> Text -> Api (Headers '[Header "ETag" String] Record)
getSingle etag uid = do
  conf <- ask
  let server = esServer conf
  let index = esIndex conf
  record <- liftIO $ getByIds server index mappingName [uid]
  case record of
    Left err -> throwError err500
    Right res ->
      case extractRecords res of
        ([], _) -> throwError err404
        (r:_, _) -> return $ addHeader (recToSha r) r

extractRecords :: SearchResult Record -> ([Record], Int)
extractRecords input = (concat $ getRecord <$> hits, B.hitsTotal result)
  where
    result = B.searchHits input
    hits = B.hits result
    getRecord = catMaybes . (: []) . B.hitSource

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
