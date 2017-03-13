{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Handlers.Xandar.Xandar as XA
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import System.Environment
import Types.Common

conf :: ApiConfig
conf =
  ApiConfig
  { apiPort = 8001
  , appName = Nothing
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
  let envDesc = withEnv dev "development mode" "production mode"
  putStrLn ("Server started on port " ++ show port ++ " in " ++ envDesc)
  let xandarConf = conf {appName = Just "xandar"}
  XA.appInit xandarConf >> runApp dev xandarConf XA.app

runApp :: Bool -> ApiConfig -> (ApiConfig -> Application) -> IO ()
runApp dev cfg app = do
  let port = fromIntegral $ apiPort cfg
  run port $ withEnv dev logStdoutDev id (app cfg)

withEnv :: Bool -> t -> t -> t
withEnv dev fd fp =
  if dev
    then fd
    else fp
