{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ^
-- All API endpoints
module Api.Main where

import Api.Lono
import Api.Xandar
import Servant

type MainApi = XandarApi :<|> LonoApi

type Documentation = "documentation.md"

type MainApiWithDocs
   = MainApi :<|> (Documentation :> Raw)

mainApiProxy :: Proxy MainApi
mainApiProxy = Proxy :: Proxy MainApi

mainApiProxyWithDocs :: Proxy MainApiWithDocs
mainApiProxyWithDocs = Proxy :: Proxy MainApiWithDocs

