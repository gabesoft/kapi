{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import qualified Handlers.Users.Xandar as XU
import Network.Wai
import Network.Wai.Handler.Warp
import Types.Common

conf =
  ApiConfig
  { apiPort = 8001
  , mongoHost = "127.0.0.1"
  , mongoPort = 27017
  , mongoDbs = Map.fromList [("xandar", "kapi-xandar")]
  }

main :: IO ()
main = do
  let port = apiPort conf
  putStrLn $ "Server started on port " ++ show port
  run (fromIntegral port) (XU.app conf)
