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

type PostApiPath = "posts"

type XandarUserApi = XandarApiPath :> UserApiPath :> GenericApi

type XandarFeedApi = XandarApiPath :> FeedApiPath :> GenericApi

type XandarPostApi = XandarApiPath :> PostApiPath :> GenericApi

apiUserProxy = Proxy :: Proxy XandarUserApi

apiFeedProxy = Proxy :: Proxy XandarFeedApi

apiPostProxy = Proxy :: Proxy XandarPostApi

apiUserProxyGetSingle =
  Proxy :: Proxy (XandarApiPath :> UserApiPath :> GetSingle)

apiFeedProxyGetSingle =
  Proxy :: Proxy (XandarApiPath :> FeedApiPath :> GetSingle)

apiPostProxyGetSingle =
  Proxy :: Proxy (XandarApiPath :> PostApiPath :> GetSingle)

apiUserProxyGetMultiple =
  Proxy :: Proxy (XandarApiPath :> UserApiPath :> GetMultiple)

apiFeedProxyGetMultiple =
  Proxy :: Proxy (XandarApiPath :> FeedApiPath :> GetMultiple)

apiPostProxyGetMultiple =
  Proxy :: Proxy (XandarApiPath :> PostApiPath :> GetMultiple)

mkUserGetSingleLink :: Text -> String
mkUserGetSingleLink = mkLink1 apiUserProxy apiUserProxyGetSingle

mkFeedGetSingleLink :: Text -> String
mkFeedGetSingleLink = mkLink1 apiFeedProxy apiFeedProxyGetSingle

mkPostGetSingleLink :: Text -> String
mkPostGetSingleLink = mkLink1 apiPostProxy apiPostProxyGetSingle

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

mkPostGetMultipleLink :: [Text]
                      -> Maybe Text
                      -> [Text]
                      -> Maybe Int
                      -> Maybe Int
                      -> String
mkPostGetMultipleLink = mkLink5 apiPostProxy apiPostProxyGetMultiple
