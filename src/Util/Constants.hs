{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Constants
module Util.Constants where

import Data.Bson

createdAtLabel :: Label
createdAtLabel = "createdAt"

updatedAtLabel :: Label
updatedAtLabel = "updatedAt"

idLabel :: Label
idLabel = "_id"

-- ^
-- Maximum number of results returned from a search or query
-- For elastic search the default for an index is 10000
-- The index max_result_window value must be increased to allow for
-- higher values
-- curl -XPUT "http://localhost:9200/kapi-xandar/_settings" -d '{ "index" : { "max_result_window" : 500000 } }'
maxResultsSize :: Int
maxResultsSize = 10000