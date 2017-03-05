{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- ^
-- Common handlers
module Handlers.Xandar.Common where

import Api.Common (GetMultiple)
import Api.Xandar
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson (encode)
import Data.Bifunctor
import Data.Bson (Field)
import Data.List (intercalate)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Database.MongoDB (Index)
import Network.HTTP.Types.Status
import Persistence.Common
import Persistence.Facade
import qualified Persistence.MongoDB as DB
import Servant
import Servant.Utils.Enter (Enter)
import Types.Common
import Util.Error

-- ^
-- Create an application that provides CRUD functionality for a record type
app'
  :: (Enter a (Api :~> Handler) (ServerT api Handler), HasServer api '[])
  => Proxy api -> a -> ApiConfig -> Application
app' proxy handlers = serve proxy . server' handlers
  where
    server'
      :: Enter a (Api :~> Handler) b
      => a -> ApiConfig -> b
    server' hs cfg = enter (toHandler cfg) hs
    toHandler :: ApiConfig -> Api :~> Handler
    toHandler conf = Nat (`runReaderT` conf)

-- ^
-- Get a single record by id
getSingle
  :: RecordDefinition
  -> Maybe Text
  -> Text
  -> Api (Headers '[ Header "ETag" String] Record)
getSingle def etag uid =
  runSingle (dbGetExisting def uid) return >>= mkGetSingleResult etag

-- ^
-- Get multiple records
getMultiple :: ApiGetMultipleLink -> RecordDefinition -> ServerT GetMultiple Api
getMultiple getLink def include query sort page perPage =
  runSingle getRecords (return . mkResult)
  where
    getRecords = getMultiple' find def include query sort page perPage
    mkResult = mkGetMultipleResult getLink include query sort perPage
    find q s fields start = runDb . DB.dbFind def q s fields start

-- ^
-- Helper for 'getMultiple'
getMultiple'
  :: (MonadBaseControl IO m, MonadReader ApiConfig m, MonadIO m)
  => ([Field] -> [Field] -> [Field] -> Int -> Int -> ExceptT ApiError m [Record])
  -> RecordDefinition
  -> [Text]
  -> Maybe Text
  -> [Text]
  -> Maybe Int
  -> Maybe Int
  -> ExceptT ApiError m ([Record], Pagination)
getMultiple' find def include query sort page perPage = do
  query' <- ExceptT (return getQuery)
  count <- runDb (DB.dbCount def query')
  let pagination = mkPagination page perPage count
  let start = paginationStart pagination
  let limit = paginationLimit pagination
  records <- find query' sort' include' start limit
  return (records, pagination)
  where
    sort' = DB.mkSortFields (splitLabels sort)
    include' = DB.mkIncludeFields (splitLabels include)
    getQuery = first mk400Err' (DB.queryToDoc def $ fromMaybe mempty query)

-- ^
-- Delete a single record
deleteSingle :: RecordDefinition -> Text -> Api NoContent
deleteSingle def uid = runSingle (dbDelete def uid) (const $ return NoContent)

-- ^
-- Create one or more records
createSingleOrMultiple
  :: RecordDefinition
  -> (Text -> String)
  -> ApiData Record
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingleOrMultiple def getLink (Single r) = createSingle def getLink r
createSingleOrMultiple def getLink (Multiple rs) = createMultiple def getLink rs

-- ^
-- Create a single record
createSingle
  :: RecordDefinition
  -> (Text -> String)
  -> Record
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createSingle def getLink input =
  runSingle (dbInsert def input) (mkCreateSingleResult getLink)

-- ^
-- Create multiple records
createMultiple
  :: RecordDefinition
  -> (Text -> String)
  -> [Record]
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
createMultiple def getLink input =
  runMulti (dbInsertMulti def input) >>= mkCreateMultipleResult getLink

-- ^
-- Update (replace) a single record
replaceSingle :: RecordDefinition -> Text -> Record -> Api Record
replaceSingle def = updateSingle (dbReplace def)

-- ^
-- Update (modify) a single record
modifySingle :: RecordDefinition -> Text -> Record -> Api Record
modifySingle def = updateSingle (dbModify def)

-- ^
-- Update (replace) multiple records
replaceMultiple :: RecordDefinition -> [Record] -> Api [ApiResult]
replaceMultiple def = runMulti . dbReplaceMulti def

-- ^
-- Update (modify) multiple records
modifyMultiple :: RecordDefinition -> [Record] -> Api [ApiResult]
modifyMultiple def = runMulti . dbModifyMulti def

-- ^
-- Handle an options request for a single record endpoint
optionsSingle :: Text
              -> Api (Headers '[ Header "Access-Control-Allow-Methods" String] NoContent)
optionsSingle _ = return $ addHeader "GET, PATCH, PUT, DELETE" NoContent

-- ^
-- Handle an options request for a multiple record endpoint
optionsMultiple :: Api (Headers '[ Header "Access-Control-Allow-Methods" String] NoContent)
optionsMultiple = return $ addHeader "GET, POST, PATCH, PUT" NoContent

-- ^
-- Add database indices
addDbIndices :: [Index] -> ApiConfig -> IO ()
addDbIndices indices conf = do
  pipe <- dbPipe conf
  mapM_ (\idx -> DB.dbAddIndex idx (confGetDbName conf) pipe) indices

-- ^
-- Run an action that could result in a single error
runSingle
  :: MonadError ServantErr m
  => ExceptT ApiError m a -> (a -> m b) -> m b
runSingle action f = do
  result <- runExceptT action
  either throwApiError f result

-- ^
-- Run an action that could result in multiple errors
runMulti
  :: Monad m
  => ApiItemsT [ApiError] m [Record] -> m [ApiResult]
runMulti action = itemsToResults <$> runApiItemsT action

-- ^
-- Modify or replace a single record
updateSingle
  :: MonadError ServantErr m
  => (Record -> ExceptT ApiError m b) -> RecordId -> Record -> m b
updateSingle f rid input = runSingle (f $ setIdValue rid input) return
-- ^
-- Create a result to be returned from a 'getSingle' method
mkGetSingleResult :: Maybe Text
                  -> Record
                  -> Api (Headers '[ Header "ETag" String] Record)
mkGetSingleResult etag record = maybe (return res) (checkEtag sha res) etag
  where
    sha = recToSha record
    res = addHeader sha record

-- ^
-- Create a result to be returned from a 'getMultiple' endpoint
mkGetMultipleResult
  :: ( AddHeader h String a orig
     , AddHeader h2 String orig orig2
     , AddHeader h4 String orig2 orig3
     , AddHeader h3 String orig3 orig1
     , AddHeader h1 String orig1 result
     )
  => ApiGetMultipleLink
  -> [Text]
  -> Maybe Text
  -> [Text]
  -> Maybe Int
  -> (a, Pagination)
  -> result
mkGetMultipleResult getLink include query sort perPage (records, pagination) =
  header paginationPage $
  header paginationTotal $
  header paginationLast $
  header paginationSize $ mkLinkHeader pagination mkUrl records
  where
    header f = addHeader (show $ f pagination)
    mkUrl p = getLink include query sort (Just p) perPage

-- ^
-- Create a result to be returned from a 'createSingle' method
mkCreateSingleResult
  :: (Text -> String)
  -> Record
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
mkCreateSingleResult getLink record =
  return $
  addHeader (getCreateLink getLink record) $ noHeader (Single $ Succ record)

-- ^
-- Create a result to be returned from a 'createMultiple' method
mkCreateMultipleResult
  :: (Text -> String)
  -> [ApiResult]
  -> Api (Headers '[ Header "Location" String, Header "Link" String] (ApiData ApiResult))
mkCreateMultipleResult getLink results =
  return . noHeader $
  addHeader (intercalate ", " $ links results) (Multiple results)
  where
    links = fmap $ apiItem (const "<>") (mkLink . getCreateLink getLink)

-- ^
-- Check an ETag against a SHA and throw a 304 error or return the output
checkEtag
  :: MonadError ServantErr m
  => String -> a -> Text -> m a
checkEtag sha output etag
  | etag == T.pack sha = throwError err304
  | otherwise = return output

-- ^
-- Create a 'Pagination' object
mkPagination :: Maybe Int -> Maybe Int -> Int -> Pagination
mkPagination page perPage =
  paginate (fromMaybe 1 page) (fromMaybe perPageDefault perPage)

-- ^
-- Create a set of pagination links to be returned in the 'Link' header
mkPaginationLinks :: Pagination -> (Int -> String) -> [String]
mkPaginationLinks pagination mkUrl =
  uncurry mkRelLink . second mkUrl <$>
  [ ("next", paginationNext pagination)
  , ("last", paginationLast pagination)
  , ("first", paginationFirst pagination)
  , ("prev", paginationPrev pagination)
  ]

-- ^
-- Create a 'Link' header containing pagination links
mkLinkHeader
  :: AddHeader h String a b
  => Pagination -> (Int -> String) -> a -> b
mkLinkHeader pagination mkUrl =
  addHeader (intercalate "," $ mkPaginationLinks pagination mkUrl)

-- ^
-- Create a link element
mkLink :: String -> String
mkLink link = "<" ++ link ++ ">"

-- ^
-- Create a relative link element
mkRelLink :: String -> String -> String
mkRelLink rel link = mkLink link ++ "; rel=\"" ++ rel ++ "\""

-- ^
-- Create a link to be returned during record creation
getCreateLink :: (Text -> String) -> Record -> String
getCreateLink getLink = getLink . fromJust . getIdValue

-- ^
-- Split a list o comma separated field names and remove all empty entries
splitLabels
  :: (Foldable t, Functor t)
  => t Text -> [Text]
splitLabels input = filter (not . T.null) $ concat (T.splitOn "," <$> input)
