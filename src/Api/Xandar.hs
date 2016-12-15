{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Declaration for all endpoints used by the Xandar app
module Api.Xandar (XandarApi, GetMultiple)where

import Data.Text (Text)
import Servant.API
import Types.Common

type XandarApi = "xandar" :> "users" :> UserApi

type GetMultiple = QueryParams "include" String
                :> QueryParam "where" String
                :> QueryParams "sort" String
                :> QueryParam "start" Int
                :> QueryParam "limit" Int
                :> Get '[JSON] (Headers '[Header "Link" String, Header "X-Total-Count" String] [Record])

type HeadNoContent = Verb HEAD 204
type OptionsNoContent = Verb OPTIONS 204

-- |
-- User api description
type UserApi =
  -- get
       GetMultiple
  :<|> Capture "id" Text :> Get '[JSON] Record
  -- delete
  :<|> Capture "id" Text :> DeleteNoContent '[JSON] NoContent
  -- create
  :<|> ReqBody '[JSON] Record :> PostCreated '[JSON] (Headers '[Header "Location" String] Record)
  :<|> ReqBody '[JSON] [Record] :> Post '[JSON] (Headers '[Header "Link" String] [ApiItem Record])
  -- update (replace)
  :<|> Capture "id" Text :> ReqBody '[JSON] Record :> Put '[JSON] Record
  :<|> ReqBody '[JSON] [Record] :> Put '[JSON] [ApiItem Record]
  -- update (modify)
  :<|> Capture "id" Text :> ReqBody '[JSON] Record :> Patch '[JSON] Record
  :<|> ReqBody '[JSON] [Record] :> Patch '[JSON] [ApiItem Record]
  -- head
  :<|> Capture "id" Text :>  HeadNoContent '[JSON] (Headers '[Header "ETag" String, Header "Last-Modified" String] NoContent)
  :<|> HeadNoContent '[JSON] (Headers '[Header "ETag" String] NoContent)
  -- options
  :<|> Capture "id" Text :> OptionsNoContent '[JSON] (Headers '[Header "Allow" String] NoContent)
  :<|> OptionsNoContent '[JSON] (Headers '[Header "Allow" String] NoContent)
