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
maxResultsSize :: Int
maxResultsSize = 10000