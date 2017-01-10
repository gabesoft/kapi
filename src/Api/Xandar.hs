{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Declaration for all endpoints used by the Xandar app
module Api.Xandar
  ( XandarUserApi
  , XandarApiPath
  , UserApiPath
  , GetMultiple
  , GetSingle
  , appName
  , apiUserProxy
  , apiFeedProxy
  , mkUserGetSingleLink
  , mkUserGetMultipleLink
  ) where

import Api.Common
import Data.Text (Text)
import Servant
import Servant.API
import Servant.Utils.Links
import Types.Common

appName :: AppName
appName = "xandar"

type XandarApiPath = "xandar"

type UserApiPath = "users"

type FeedApiPath = "feeds"

type XandarUserApi = XandarApiPath :> UserApiPath :> GenericApi

type XandarFeedApi = XandarApiPath :> FeedApiPath :> GenericApi

apiUserProxy = Proxy :: Proxy XandarUserApi

apiFeedProxy = Proxy :: Proxy XandarFeedApi

apiUserProxyGetSingle =
  Proxy :: Proxy (XandarApiPath :> UserApiPath :> GetSingle)

apiFeedProxyGetSingle =
  Proxy :: Proxy (XandarApiPath :> FeedApiPath :> GetSingle)

apiUserProxyGetMultiple =
  Proxy :: Proxy (XandarApiPath :> UserApiPath :> GetMultiple)

apiFeedProxyGetMultiple =
  Proxy :: Proxy (XandarApiPath :> FeedApiPath :> GetMultiple)

mkUserGetSingleLink :: Text -> String
mkUserGetSingleLink = mkLink1 apiUserProxy apiUserProxyGetSingle

mkFeedGetSingleLink :: Text -> String
mkFeedGetSingleLink = mkLink1 apiFeedProxy apiFeedProxyGetSingle

mkUserGetMultipleLink :: [Text]
                      -> Maybe Text
                      -> [Text]
                      -> Maybe Int
                      -> Maybe Int
                      -> String
mkUserGetMultipleLink = mkLink5 apiUserProxy apiUserProxyGetMultiple

mkFeedGetMultipleLink :: [Text]
                      -> Maybe Text
                      -> [Text]
                      -> Maybe Int
                      -> Maybe Int
                      -> String
mkFeedGetMultipleLink = mkLink5 apiFeedProxy apiFeedProxyGetMultiple
