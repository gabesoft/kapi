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
import Types.Xandar

type XandarApi = "xandar" :> "users" :> UserApi

type GetMultiple = QueryParams "include" String
                :> QueryParam "where" String
                :> QueryParams "sort" String
                :> QueryParam "start" Int
                :> QueryParam "limit" Int
                :> Get '[JSON] (Headers '[Header "Link" String, Header "X-Total-Count" String] [User])

type HeadNoContent = Verb HEAD 204
type OptionsNoContent = Verb OPTIONS 204

-- |
-- User api description
type UserApi =
  -- | get
       GetMultiple
  :<|> Capture "id" UserId :> Get '[JSON] User
  -- | delete
  :<|> Capture "id" UserId :> DeleteNoContent '[JSON] NoContent
  -- | create
  :<|> ReqBody '[JSON] User :> PostCreated '[JSON] (Headers '[Header "Location" String] User)
  :<|> ReqBody '[JSON] [User] :> Post '[JSON] (Headers '[Header "Link" String] [ModelOrError User])
  -- | update (replace)
  :<|> Capture "id" UserId :> ReqBody '[JSON] User :> Put '[JSON] User
  :<|> ReqBody '[JSON] [User] :> Put '[JSON] [ModelOrError User]
  -- | update (modify)
  :<|> Capture "id" UserId :> ReqBody '[JSON] User :> Patch '[JSON] User
  :<|> ReqBody '[JSON] [User] :> Patch '[JSON] [ModelOrError User]
  -- | head
  :<|> Capture "id" UserId :>  HeadNoContent '[JSON] (Headers '[Header "ETag" String, Header "Last-Modified" String] NoContent)
  :<|> HeadNoContent '[JSON] (Headers '[Header "ETag" String] NoContent)
  -- | options
  :<|> Capture "id" UserId :> OptionsNoContent '[JSON] (Headers '[Header "Allow" String] NoContent)
  :<|> OptionsNoContent '[JSON] (Headers '[Header "Allow" String] NoContent)
