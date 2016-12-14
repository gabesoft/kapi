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
import Persistence.Users.Xandar
import Servant.API
import Types.Xandar

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
  :<|> Capture "id" UserId :> Get '[JSON] Record
  -- delete
  :<|> Capture "id" UserId :> DeleteNoContent '[JSON] NoContent
  -- create
  :<|> ReqBody '[JSON] Record :> PostCreated '[JSON] (Headers '[Header "Location" String] Record)
  :<|> ReqBody '[JSON] [Record] :> Post '[JSON] (Headers '[Header "Link" String] [ModelOrError Record])
  -- update (replace)
  :<|> Capture "id" UserId :> ReqBody '[JSON] Record :> Put '[JSON] Record
  :<|> ReqBody '[JSON] [Record] :> Put '[JSON] [ModelOrError Record]
  -- update (modify)
  :<|> Capture "id" UserId :> ReqBody '[JSON] Record :> Patch '[JSON] Record
  :<|> ReqBody '[JSON] [Record] :> Patch '[JSON] [ModelOrError Record]
  -- head
  :<|> Capture "id" UserId :>  HeadNoContent '[JSON] (Headers '[Header "ETag" String, Header "Last-Modified" String] NoContent)
  :<|> HeadNoContent '[JSON] (Headers '[Header "ETag" String] NoContent)
  -- options
  :<|> Capture "id" UserId :> OptionsNoContent '[JSON] (Headers '[Header "Allow" String] NoContent)
  :<|> OptionsNoContent '[JSON] (Headers '[Header "Allow" String] NoContent)
