{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ^
-- Lono API endpoints
module Api.Lono where

import Api.Common
import Data.Text (Text)
import Servant

type LonoApiPath = "lono"

type UserApiPath = "users"

type FeedApiPath = "feeds"

type PostApiPath = "posts"

type UserPostApiPath = "user-posts"

type SubscriptionApiPath = "subscriptions"

type PostQueryApiPath = "post-queries"

type TagsApiPath = "tags"

type LonoUserApi = LonoApiPath :> UserApiPath :> GenericApi

type LonoFeedApi = LonoApiPath :> FeedApiPath :> GenericApi

type LonoPostApi = LonoApiPath :> PostApiPath :> GenericApi

type LonoUserPostApi = LonoApiPath :> UserPostApiPath :> GenericApi

type LonoSubscriptionApi = LonoApiPath :> SubscriptionApiPath :> GenericApi

type LonoPostQueryApi = LonoApiPath :> PostQueryApiPath :> GenericApi

type LonoTagsApi = LonoApiPath :> TagsApiPath :> GenericApi

type LonoApi = LonoUserApi
            :<|> LonoFeedApi
            :<|> LonoPostApi
            :<|> LonoSubscriptionApi
            :<|> LonoUserPostApi
            :<|> LonoPostQueryApi
            :<|> LonoTagsApi

apiProxy :: Proxy LonoApi
apiProxy = Proxy :: Proxy LonoApi

apiUserProxy :: Proxy LonoUserApi
apiUserProxy = Proxy :: Proxy LonoUserApi

apiFeedProxy :: Proxy LonoFeedApi
apiFeedProxy = Proxy :: Proxy LonoFeedApi

apiPostProxy :: Proxy LonoPostApi
apiPostProxy = Proxy :: Proxy LonoPostApi

apiUserPostProxy :: Proxy LonoUserPostApi
apiUserPostProxy = Proxy :: Proxy LonoUserPostApi

apiSubscriptionProxy :: Proxy LonoSubscriptionApi
apiSubscriptionProxy = Proxy :: Proxy LonoSubscriptionApi

apiPostQueryProxy :: Proxy LonoPostQueryApi
apiPostQueryProxy = Proxy :: Proxy LonoPostQueryApi

apiTagsProxy :: Proxy LonoTagsApi
apiTagsProxy = Proxy :: Proxy LonoTagsApi

apiUserProxyGetSingle
  :: Proxy (LonoApiPath :> (UserApiPath :> GetSingle))
apiUserProxyGetSingle =
  Proxy :: Proxy (LonoApiPath :> UserApiPath :> GetSingle)

apiFeedProxyGetSingle
  :: Proxy (LonoApiPath :> (FeedApiPath :> GetSingle))
apiFeedProxyGetSingle =
  Proxy :: Proxy (LonoApiPath :> FeedApiPath :> GetSingle)

apiPostProxyGetSingle
  :: Proxy (LonoApiPath :> (PostApiPath :> GetSingle))
apiPostProxyGetSingle =
  Proxy :: Proxy (LonoApiPath :> PostApiPath :> GetSingle)

apiUserPostProxyGetSingle
  :: Proxy (LonoApiPath :> (UserPostApiPath :> GetSingle))
apiUserPostProxyGetSingle =
  Proxy :: Proxy (LonoApiPath :> UserPostApiPath :> GetSingle)

apiSubscriptionProxyGetSingle
  :: Proxy (LonoApiPath :> (SubscriptionApiPath :> GetSingle))
apiSubscriptionProxyGetSingle =
  Proxy :: Proxy (LonoApiPath :> SubscriptionApiPath :> GetSingle)

apiPostQueryProxyGetSingle
  :: Proxy (LonoApiPath :> (PostQueryApiPath :> GetSingle))
apiPostQueryProxyGetSingle =
  Proxy :: Proxy (LonoApiPath :> PostQueryApiPath :> GetSingle)

apiTagsProxyGetSingle
  :: Proxy (LonoApiPath :> (TagsApiPath :> GetSingle))
apiTagsProxyGetSingle =
  Proxy :: Proxy (LonoApiPath :> TagsApiPath :> GetSingle)

apiUserProxyGetMultiple
  :: Proxy (LonoApiPath :> (UserApiPath :> GetMultiple))
apiUserProxyGetMultiple =
  Proxy :: Proxy (LonoApiPath :> UserApiPath :> GetMultiple)

apiFeedProxyGetMultiple
  :: Proxy (LonoApiPath :> (FeedApiPath :> GetMultiple))
apiFeedProxyGetMultiple =
  Proxy :: Proxy (LonoApiPath :> FeedApiPath :> GetMultiple)

apiPostProxyGetMultiple
  :: Proxy (LonoApiPath :> (PostApiPath :> GetMultiple))
apiPostProxyGetMultiple =
  Proxy :: Proxy (LonoApiPath :> PostApiPath :> GetMultiple)

apiUserPostProxyGetMultiple
  :: Proxy (LonoApiPath :> (UserPostApiPath :> GetMultiple))
apiUserPostProxyGetMultiple =
  Proxy :: Proxy (LonoApiPath :> UserPostApiPath :> GetMultiple)

apiSubscriptionProxyGetMultiple
  :: Proxy (LonoApiPath :> (SubscriptionApiPath :> GetMultiple))
apiSubscriptionProxyGetMultiple =
  Proxy :: Proxy (LonoApiPath :> SubscriptionApiPath :> GetMultiple)

apiPostQueryProxyGetMultiple
  :: Proxy (LonoApiPath :> (PostQueryApiPath :> GetMultiple))
apiPostQueryProxyGetMultiple =
  Proxy :: Proxy (LonoApiPath :> PostQueryApiPath :> GetMultiple)

apiTagsProxyGetMultiple
  :: Proxy (LonoApiPath :> (TagsApiPath :> GetMultiple))
apiTagsProxyGetMultiple =
  Proxy :: Proxy (LonoApiPath :> TagsApiPath :> GetMultiple)

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

mkPostQueryGetSingleLink :: Text -> String
mkPostQueryGetSingleLink = mkLink1 apiPostQueryProxy apiPostQueryProxyGetSingle

mkTagsGetSingleLink :: Text -> String
mkTagsGetSingleLink = mkLink1 apiTagsProxy apiTagsProxyGetSingle

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

mkPostQueryGetMultipleLink :: ApiGetMultipleLink
mkPostQueryGetMultipleLink = mkLink5 apiPostQueryProxy apiPostQueryProxyGetMultiple

mkTagsGetMultipleLink :: ApiGetMultipleLink
mkTagsGetMultipleLink = mkLink5 apiTagsProxy apiTagsProxyGetMultiple
