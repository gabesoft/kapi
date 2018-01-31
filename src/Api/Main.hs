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

type MainApi = LonoApi :<|> XandarApi

mainProxy :: Proxy MainApi
mainProxy = Proxy :: Proxy MainApi

