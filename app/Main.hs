{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Handlers.Xandar.Users as XU
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import System.Environment
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
  env <- lookupEnv "KAPI_ENV"
  let port = apiPort conf
  let dev = fromMaybe False $ (== "development") <$> env
  let env = if dev
       then "development mode"
       else "production mode"
  putStrLn ("Server started on port " ++ show port ++ " in " ++ env)
  XU.appInit conf
  if dev
    then run (fromIntegral port) $ logStdoutDev (XU.app conf)
    else run (fromIntegral port) (XU.app conf)
