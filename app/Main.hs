{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Handlers.Xandar.Xandar as XX
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
  , esServer = "http://localhost:9200"
  , esIndices = Map.fromList [("xandar", "kapi-xandar")]
  }

main :: IO ()
main = do
  env <- lookupEnv "KAPI_ENV"
  let port = apiPort conf
  let dev = fromMaybe False $ (== "development") <$> env
  let env = withEnv dev "development mode" "production mode"
  putStrLn ("Server started on port " ++ show port ++ " in " ++ env)
  XX.appInit conf >> runApp dev conf XX.app

runApp :: Bool -> ApiConfig -> (ApiConfig -> Application) -> IO ()
runApp dev conf app = do
  let port = fromIntegral $ apiPort conf
  run port $ withEnv dev logStdoutDev id (app conf)

withEnv :: Bool -> t -> t -> t
withEnv dev fd fp =
  if dev
    then fd
    else fp
