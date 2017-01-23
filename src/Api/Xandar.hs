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
  , XandarPostApi
  , XandarFeedApi
  , XandarSubscriptionApi
  , XandarApiPath
  , XandarApi
  , UserApiPath
  , GetMultiple
  , GetSingle
  , ApiGetMultipleLink
  , appName
  , apiProxy
  , apiUserProxy
  , apiFeedProxy
  , apiPostProxy
  , apiSubscriptionProxy
  , mkUserGetSingleLink
  , mkUserGetMultipleLink
  , mkFeedGetSingleLink
  , mkFeedGetMultipleLink
  , mkPostGetSingleLink
  , mkPostGetMultipleLink
  , mkSubscriptionGetSingleLink
  , mkSubscriptionGetMultipleLink
  ) where

import Api.Common
import Data.Text (Text)
import Servant
import Types.Common

appName :: AppName
appName = "xandar"

type XandarApiPath = "xandar"

type UserApiPath = "users"

type FeedApiPath = "feeds"

type SubscriptionApiPath = "subscriptions"

type PostApiPath = "posts"

type UserPostApiPath = "user-posts"

type XandarUserApi = XandarApiPath :> UserApiPath :> GenericApi

type XandarFeedApi = XandarApiPath :> FeedApiPath :> GenericApi

type XandarPostApi = XandarApiPath :> PostApiPath :> GenericApi

type XandarUserPostApi = XandarApiPath :> UserPostApiPath :> GenericApi

type XandarSubscriptionApi = XandarApiPath :> SubscriptionApiPath :> GenericApi

type XandarApi = XandarUserApi :<|> XandarFeedApi :<|> XandarPostApi :<|> XandarSubscriptionApi :<|> XandarUserPostApi

type ApiGetMultipleLink = [Text] -> Maybe Text -> [Text] -> Maybe Int -> Maybe Int -> String

apiProxy :: Proxy XandarApi
apiProxy = Proxy :: Proxy XandarApi

apiUserProxy :: Proxy XandarUserApi
apiUserProxy = Proxy :: Proxy XandarUserApi

apiFeedProxy :: Proxy XandarFeedApi
apiFeedProxy = Proxy :: Proxy XandarFeedApi

apiPostProxy :: Proxy XandarPostApi
apiPostProxy = Proxy :: Proxy XandarPostApi

apiSubscriptionProxy :: Proxy XandarSubscriptionApi
apiSubscriptionProxy = Proxy :: Proxy XandarSubscriptionApi

apiUserProxyGetSingle
  :: Proxy (XandarApiPath :> (UserApiPath :> GetSingle))
apiUserProxyGetSingle =
  Proxy :: Proxy (XandarApiPath :> UserApiPath :> GetSingle)

apiFeedProxyGetSingle
  :: Proxy (XandarApiPath :> (FeedApiPath :> GetSingle))
apiFeedProxyGetSingle =
  Proxy :: Proxy (XandarApiPath :> FeedApiPath :> GetSingle)

apiPostProxyGetSingle
  :: Proxy (XandarApiPath :> (PostApiPath :> GetSingle))
apiPostProxyGetSingle =
  Proxy :: Proxy (XandarApiPath :> PostApiPath :> GetSingle)

apiSubscriptionProxyGetSingle
  :: Proxy (XandarApiPath :> (SubscriptionApiPath :> GetSingle))
apiSubscriptionProxyGetSingle =
  Proxy :: Proxy (XandarApiPath :> SubscriptionApiPath :> GetSingle)

apiUserProxyGetMultiple
  :: Proxy (XandarApiPath :> (UserApiPath :> GetMultiple))
apiUserProxyGetMultiple =
  Proxy :: Proxy (XandarApiPath :> UserApiPath :> GetMultiple)

apiFeedProxyGetMultiple
  :: Proxy (XandarApiPath :> (FeedApiPath :> GetMultiple))
apiFeedProxyGetMultiple =
  Proxy :: Proxy (XandarApiPath :> FeedApiPath :> GetMultiple)

apiPostProxyGetMultiple
  :: Proxy (XandarApiPath :> (PostApiPath :> GetMultiple))
apiPostProxyGetMultiple =
  Proxy :: Proxy (XandarApiPath :> PostApiPath :> GetMultiple)

apiSubscriptionProxyGetMultiple
  :: Proxy (XandarApiPath :> (SubscriptionApiPath :> GetMultiple))
apiSubscriptionProxyGetMultiple =
  Proxy :: Proxy (XandarApiPath :> SubscriptionApiPath :> GetMultiple)

mkUserGetSingleLink :: Text -> String
mkUserGetSingleLink = mkLink1 apiUserProxy apiUserProxyGetSingle

mkFeedGetSingleLink :: Text -> String
mkFeedGetSingleLink = mkLink1 apiFeedProxy apiFeedProxyGetSingle

mkPostGetSingleLink :: Text -> String
mkPostGetSingleLink = mkLink1 apiPostProxy apiPostProxyGetSingle

mkSubscriptionGetSingleLink :: Text -> String
mkSubscriptionGetSingleLink = mkLink1 apiSubscriptionProxy apiSubscriptionProxyGetSingle

mkUserGetMultipleLink :: ApiGetMultipleLink
mkUserGetMultipleLink = mkLink5 apiUserProxy apiUserProxyGetMultiple

mkFeedGetMultipleLink :: ApiGetMultipleLink
mkFeedGetMultipleLink = mkLink5 apiFeedProxy apiFeedProxyGetMultiple

mkPostGetMultipleLink :: ApiGetMultipleLink
mkPostGetMultipleLink = mkLink5 apiPostProxy apiPostProxyGetMultiple

mkSubscriptionGetMultipleLink :: ApiGetMultipleLink
mkSubscriptionGetMultipleLink = mkLink5 apiSubscriptionProxy apiSubscriptionProxyGetMultiple
