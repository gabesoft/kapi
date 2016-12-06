{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Documentation generation for Xandar endpoints
module Doc.Xandar (markdownDoc) where

import Data.Text (Text)
import Servant.API
import Servant.Docs
import Data.Proxy
import Types.Xandar
import Api.Xandar

instance ToSample User where
    toSamples _ = singleSample u1

instance ToSample Error where
    toSamples _ = singleSample (Error "something bad happened")

instance ToSample (ModelOrError User) where
    toSamples _ = singleSample (Succ u1)

instance ToCapture (Capture "id" Text) where
    toCapture _ = DocCapture "id" "user identifier"

instance ToParam (QueryParams "include" String) where
    toParam _ =
        DocQueryParam
            "include"
            ["name", "profile.description"]
            "a list of fields to be included in the objects returned"
            List

instance ToParam (QueryParam "where" String) where
    toParam _ =
        DocQueryParam
            "where"
            ["(name eq 'Sherlock') or (profile.date lt '12999888')"]
            "record filters"
            Normal

instance ToParam (QueryParams "sort" String) where
    toParam _ =
        DocQueryParam
            "sort"
            ["+name", "-createdAt"]
            "sort fields +ascending or -descending"
            List

instance ToParam (QueryParam "start" Int) where
    toParam _ = DocQueryParam "start" ["0"] "start index for pagination" Normal

instance ToParam (QueryParam "limit" Int) where
    toParam _ =
        DocQueryParam
            "limit"
            ["50"]
            "the number of records to return (page size)"
            Normal

instance ToSample Char where
    toSamples _ = singleSample ' '

instance ToSample Int where
    toSamples _ = singleSample 0

markdownDoc :: String
markdownDoc = markdown $ docs (Proxy :: Proxy XandarApi)