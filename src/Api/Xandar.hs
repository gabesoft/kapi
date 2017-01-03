{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Declaration for all endpoints used by the Xandar app
module Api.Xandar
  ( XandarApi
  , XandarApiPre
  , UserApiPre
  , GetMultiple
  , GetSingle
  , apiProxy
  , mkGetSingleLink
  , mkGetMultipleLink
  ) where

import Data.Text (Text)
import Servant
import Servant.API
import Servant.Utils.Links
import Types.Common

type UserApiPre = "users"

type XandarApiPre = "xandar"

type XandarApi = XandarApiPre :> UserApiPre :> UserApi

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
-- User api description
type UserApi =
  -- get
       GetMultiple
  :<|> Header "If-None-Match" Text :> GetSingle
  -- delete
  :<|> Capture "id" Text :> DeleteNoContent '[JSON] NoContent
  -- create
  :<|> ReqBody '[JSON] (ApiData Record) :> PostCreated '[JSON] (Headers '[Header "Location" String, Header "Link" String] (ApiData (ApiItem ApiError Record)))
  -- update (replace)
  :<|> Capture "id" Text :> ReqBody '[JSON] Record :> Put '[JSON] Record
  :<|> ReqBody '[JSON] [Record] :> Put '[JSON] [ApiItem ApiError Record]
  -- update (modify)
  :<|> Capture "id" Text :> ReqBody '[JSON] Record :> Patch '[JSON] Record
  :<|> ReqBody '[JSON] [Record] :> Patch '[JSON] [ApiItem ApiError Record]
  -- head
  :<|> Capture "id" Text :>  HeadNoContent '[JSON] (Headers '[Header "ETag" String] NoContent)
  :<|> HeadNoContent '[JSON] (Headers '[Header "X-Total-Count" String] NoContent)
  -- options
  :<|> Capture "id" Text :> OptionsNoContent '[JSON] (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)
  :<|> OptionsNoContent '[JSON] (Headers '[Header "Access-Control-Allow-Methods" String] NoContent)

apiProxy = Proxy :: Proxy XandarApi

apiProxyGetSingle = Proxy :: Proxy (XandarApiPre :> UserApiPre :> GetSingle)
apiProxyGetMultiple = Proxy :: Proxy (XandarApiPre :> UserApiPre :> GetMultiple)

mkGetSingleLink :: Text -> String
mkGetSingleLink = ("/" ++) . show . safeLink apiProxy apiProxyGetSingle

mkGetMultipleLink :: [Text]
                  -> Maybe Text
                  -> [Text]
                  -> Maybe Int
                  -> Maybe Int
                  -> String
mkGetMultipleLink include query sort page perPage =
  ("/" ++) $
  show $ safeLink apiProxy apiProxyGetMultiple include query sort page perPage
