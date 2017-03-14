{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import Data.Monoid ((<>))
import qualified Handlers.Xandar.Xandar as XA
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import System.Environment
import Types.Common

main :: IO ()
main = do
  args <- getArgs
  conf <- getConfig args
  env <- lookupEnv "KAPI_ENV"
  let port = apiPort conf
  let dev = fromMaybe False $ (== "development") <$> env
  let envDesc = withEnv dev "development mode" "production mode"
  putStrLn ("Server started on port " ++ show port ++ " in " ++ envDesc)
  let xandarConf = conf {appName = Just "xandar"}
  XA.appInit xandarConf >> runApp dev xandarConf XA.app

getConfig :: [String] -> IO ApiConfig
getConfig [] = readConfigFile "./config/default.json"
getConfig (f:_) = readConfigFile f

readConfigFile :: String -> IO ApiConfig
readConfigFile f = do
  putStrLn $ "Config file " <> f
  json <- L.readFile f
  return $ fromJust (A.decode json)

runApp :: Bool -> ApiConfig -> (ApiConfig -> Application) -> IO ()
runApp dev conf app = do
  let port = fromIntegral $ apiPort conf
  run port $ withEnv dev logStdoutDev id (app conf)

withEnv :: Bool -> t -> t -> t
withEnv dev fd fp =
  if dev
    then fd
    else fp
