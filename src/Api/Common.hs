{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- ^ Common API definitions
module Api.Common where

import Data.Text (Text, unpack)
import Servant
import Types.Common

type GetMultiple = QueryParams "include" Text
                :> QueryParam "where" Text
                :> QueryParams "sort" Text
                :> QueryParam "page" Int
                :> QueryParam "per_page" Int
                :> Get '[JSON] (Headers '[ Header "X-Page" String
                                         , Header "X-Total-Count" String
                                         , Header "X-Page-Count" String
                                         , Header "X-Per-Page" String
                                         , Header "Link" String
                                         ] [Record])

type GetSingle = Capture "id" Text :> Get '[JSON] (Headers '[Header "ETag" String] Record)

type HeadNoContent = Verb 'HEAD 204
type OptionsNoContent = Verb 'OPTIONS 204

-- |
-- Generic API description
type GenericApi =
  -- get
       GetMultiple
  :<|> Header "If-None-Match" Text :> GetSingle
  -- delete
  :<|> Capture "id" Text :> DeleteNoContent '[JSON] NoContent
  -- create
  :<|> ReqBody '[JSON] (ApiData Record) :> PostCreated '[JSON] (Headers '[Header "Location" String, Header "Link" String] (ApiData ApiResult))
  -- update (replace)
  :<|> Capture "id" Text :> ReqBody '[JSON] Record :> Put '[JSON] Record
  :<|> ReqBody '[JSON] [Record] :> Put '[JSON] [ApiResult]
  -- update (modify)
  :<|> Capture "id" Text :> ReqBody '[JSON] Record :> Patch '[JSON] Record
  :<|> ReqBody '[JSON] [Record] :> Patch '[JSON] [ApiResult]
  -- options
  :<|> Capture "id" Text :> OptionsNoContent '[JSON] (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
  :<|> OptionsNoContent '[JSON] (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)

type ApiGetMultipleLink = [Text] -> Maybe Text -> [Text] -> Maybe Int -> Maybe Int -> String

perPageDefault :: Int
perPageDefault = 50

mkLink1
  :: (MkLink endpoint ~ (a -> x), HasLink endpoint, ToHttpApiData x, IsElem endpoint api)
  => Proxy api -> Proxy endpoint -> a -> String
mkLink1 api method = mkPath . safeLink api method

mkLink2
  :: (MkLink endpoint ~ (a -> b -> x)
     ,HasLink endpoint
     ,ToHttpApiData x
     ,IsElem endpoint api)
  => Proxy api -> Proxy endpoint -> a -> b -> String
mkLink2 api method a = mkPath . safeLink api method a

mkLink3
  :: (MkLink endpoint ~ (a -> b -> c -> x)
     ,HasLink endpoint
     ,ToHttpApiData x
     ,IsElem endpoint api)
  => Proxy api -> Proxy endpoint -> a -> b -> c -> String
mkLink3 api method a b = mkPath . safeLink api method a b

mkLink4
  :: (MkLink endpoint ~ (a -> b -> c -> d -> x)
     ,HasLink endpoint
     ,ToHttpApiData x
     ,IsElem endpoint api)
  => Proxy api -> Proxy endpoint -> a -> b -> c -> d -> String
mkLink4 api method a b c = mkPath . safeLink api method a b c

mkLink5
  :: (MkLink endpoint ~ (a -> b -> c -> d -> e -> x)
     ,HasLink endpoint
     ,ToHttpApiData x
     ,IsElem endpoint api)
  => Proxy api -> Proxy endpoint -> a -> b -> c -> d -> e -> String
mkLink5 api method a b c d = mkPath . safeLink api method a b c d

mkLink6
  :: (MkLink endpoint ~ (a -> b -> c -> d -> e -> f -> x)
     ,HasLink endpoint
     ,ToHttpApiData x
     ,IsElem endpoint api)
  => Proxy api -> Proxy endpoint -> a -> b -> c -> d -> e -> f -> String
mkLink6 api method a b c d e = mkPath . safeLink api method a b c d e

mkPath :: ToHttpApiData a => a -> String
mkPath = ("/" ++) . unpack . toUrlPiece