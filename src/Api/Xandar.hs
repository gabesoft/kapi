{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Endpoint declarations
module Api.Xandar (XandarApi, xandarApiDocs)where

import Data.Text (Text)
import Data.Proxy
import Servant.API
import Servant.Docs
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

type UserApi = GetMultiple
          :<|> Capture "id" UserId :> Get '[JSON] User
          -- delete
          :<|> Capture "id" UserId :> DeleteNoContent '[JSON] NoContent
          -- create
          :<|> ReqBody '[JSON] User :> PostCreated '[JSON] (Headers '[Header "Location" String] User)
          :<|> ReqBody '[JSON] [User] :> Post '[JSON] (Headers '[Header "Link" String] [ModelOrError User])
          -- update (replace)
          :<|> Capture "id" UserId :> ReqBody '[JSON] User :> Put '[JSON] User
          :<|> ReqBody '[JSON] [User] :> Put '[JSON] [ModelOrError User]
          -- update (modify)
          :<|> Capture "id" UserId :> ReqBody '[JSON] User :> Patch '[JSON] User
          :<|> ReqBody '[JSON] [User] :> Patch '[JSON] [ModelOrError User]
          -- head
          :<|> Capture "id" UserId :>  HeadNoContent '[JSON] (Headers '[Header "ETag" String, Header "Last-Modified" String] NoContent)
          :<|> HeadNoContent '[JSON] (Headers '[Header "ETag" String] NoContent)
          -- options
          :<|> Capture "id" UserId :> OptionsNoContent '[JSON] (Headers '[Header "Allow" String] NoContent)
          :<|> OptionsNoContent '[JSON] (Headers '[Header "Allow" String] NoContent)

xandarApi :: Proxy XandarApi
xandarApi = Proxy

instance ToSample User where
  toSamples _ = singleSample u1

instance ToSample Error where
  toSamples _ = singleSample (Error "something bad happened")

instance ToSample (ModelOrError User) where
  toSamples _ = singleSample (Succ u1)

instance ToCapture (Capture "id" Text) where
  toCapture _ = DocCapture "id" "user identifier"

instance ToParam (QueryParams "include" String) where
  toParam _ = DocQueryParam "include" [ "name", "profile.description" ] "a list of fields to be included in the objects returned" List

instance ToParam (QueryParam "where" String) where
  toParam _ = DocQueryParam "where" [ "(name eq 'Sherlock') or (profile.date lt 12999888)"] "record filters" Normal

instance ToParam (QueryParams "sort" String) where
  toParam _ = DocQueryParam "sort" [ "+name", "-createdAt" ] "sort fields (prefix with '-' for descending sort)" List

instance ToParam (QueryParam "start" Int) where
  toParam _ = DocQueryParam "start" ["0"] "start index for pagination" Normal

instance ToParam (QueryParam "limit" Int) where
  toParam _ = DocQueryParam "limit" ["50"] "the number of records to return (page size)" Normal

instance ToSample Char where
  toSamples _ = singleSample ' '

instance ToSample Int where
  toSamples _ = singleSample 0

xandarApiDocs :: String
xandarApiDocs = markdown $ docs xandarApi
