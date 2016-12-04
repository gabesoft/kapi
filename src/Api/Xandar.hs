{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Endpoint declarations
module Api.Xandar where

import Data.Proxy
import Servant.API
import Servant.Docs
import Types.Xandar

type XandarApi = "xandar" :> Api

type P = Post '[JSON]

type UP a = "users" :> P a

type Api = "users" :> Capture "id" String :> Get '[JSON] User :<|> UP User :<|> UP [User]

xandarApi :: Proxy XandarApi
xandarApi = Proxy

instance ToSample User where
  toSamples _ = singleSample u1

instance ToCapture (Capture "id" String) where
  toCapture _ = DocCapture "id" "user identifier"

apiDocs :: String
apiDocs = markdown $ docs xandarApi
