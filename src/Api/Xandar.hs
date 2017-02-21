{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Declaration for all endpoints used by the Xandar app
module Api.Xandar
  ( ApiGetMultipleLink
  , GetMultiple
  , GetSingle
  , UserApiPath
  , XandarApi
  , XandarApiPath
  , XandarFeedApi
  , XandarPostApi
  , XandarSubscriptionApi
  , XandarUserApi
  , XandarUserPostApi
  , apiFeedProxy
  , apiPostProxy
  , apiProxy
  , apiSubscriptionProxy
  , apiUserPostProxy
  , apiUserProxy
  , appName
  , mkFeedGetMultipleLink
  , mkFeedGetSingleLink
  , mkPostGetMultipleLink
  , mkPostGetSingleLink
  , mkSubscriptionGetMultipleLink
  , mkSubscriptionGetSingleLink
  , mkUserGetMultipleLink
  , mkUserGetSingleLink
  , mkUserPostGetMultipleLink
  , mkUserPostGetSingleLink
  , perPageDefault
  ) where

import Api.Common
import Data.Text (Text)
import Servant
import Types.Common

perPageDefault :: Int
perPageDefault = 50

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

apiUserPostProxy :: Proxy XandarUserPostApi
apiUserPostProxy = Proxy :: Proxy XandarUserPostApi

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

apiUserPostProxyGetSingle
  :: Proxy (XandarApiPath :> (UserPostApiPath :> GetSingle))
apiUserPostProxyGetSingle =
  Proxy :: Proxy (XandarApiPath :> UserPostApiPath :> GetSingle)

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

apiUserPostProxyGetMultiple
  :: Proxy (XandarApiPath :> (UserPostApiPath :> GetMultiple))
apiUserPostProxyGetMultiple =
  Proxy :: Proxy (XandarApiPath :> UserPostApiPath :> GetMultiple)

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

mkUserPostGetSingleLink :: Text -> String
mkUserPostGetSingleLink = mkLink1 apiUserPostProxy apiUserPostProxyGetSingle

mkSubscriptionGetSingleLink :: Text -> String
mkSubscriptionGetSingleLink = mkLink1 apiSubscriptionProxy apiSubscriptionProxyGetSingle

mkUserGetMultipleLink :: ApiGetMultipleLink
mkUserGetMultipleLink = mkLink5 apiUserProxy apiUserProxyGetMultiple

mkFeedGetMultipleLink :: ApiGetMultipleLink
mkFeedGetMultipleLink = mkLink5 apiFeedProxy apiFeedProxyGetMultiple

mkPostGetMultipleLink :: ApiGetMultipleLink
mkPostGetMultipleLink = mkLink5 apiPostProxy apiPostProxyGetMultiple

mkUserPostGetMultipleLink :: ApiGetMultipleLink
mkUserPostGetMultipleLink = mkLink5 apiUserPostProxy apiUserPostProxyGetMultiple

mkSubscriptionGetMultipleLink :: ApiGetMultipleLink
mkSubscriptionGetMultipleLink = mkLink5 apiSubscriptionProxy apiSubscriptionProxyGetMultiple
