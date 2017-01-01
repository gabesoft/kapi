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
  ) where

import Data.Text (Text)
import Servant
import Servant.API
import Servant.Utils.Links
import Types.Common

type UserApiPre = "users"

type XandarApiPre = "xandar"

type XandarApi = XandarApiPre :> UserApiPre :> UserApi

type GetMultiple = QueryParams "include" String
                :> QueryParam "where" String
                :> QueryParams "sort" String
                :> QueryParam "start" Int
                :> QueryParam "limit" Int
                :> Get '[JSON] (Headers '[Header "Link" String, Header "X-Total-Count" String] [Record])

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
  :<|> Capture "id" Text :> OptionsNoContent '[JSON] (Headers '[Header "Allow" String] NoContent)
  :<|> OptionsNoContent '[JSON] (Headers '[Header "Allow" String] NoContent)

apiProxy = Proxy :: Proxy XandarApi

apiProxyGetSingle = Proxy :: Proxy (XandarApiPre :> UserApiPre :> GetSingle)

mkGetSingleLink :: Text -> String
mkGetSingleLink = ("/" ++) . show . safeLink apiProxy apiProxyGetSingle
