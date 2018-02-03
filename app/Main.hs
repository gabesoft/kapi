{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Handlers.Lono as LO
import qualified Handlers.Main as MA
import qualified Handlers.Xandar as XA
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
  let port = appPort conf
  let dev = (== Just "development") env
  let envDesc = withEnv dev "development mode" "production mode"
  putStrLn ("Server started on port " ++ show port ++ " in " ++ envDesc)
  let xandarConf = getApiConfig "xandar" conf
  let lonoConf = getApiConfig "lono" conf
  LO.appInit lonoConf
  XA.appInit xandarConf
  runApp dev (MA.app xandarConf lonoConf) conf

getApiConfig :: ApiName -> AppConfig -> ApiConfig
getApiConfig apiName conf =
  ApiConfig
    { apiPort = appPort conf
    , mongoServer = getApiValue (appMongoServers conf) apiName
    , mongoDatabase = getApiValue (appMongoDbs conf) apiName
    , esIndex = getApiValue (appEsIndices conf) apiName
    , esServer = getApiValue (appEsServers conf) apiName
    }

getApiValue :: Show a => Map.Map ApiName a -> ApiName -> a
getApiValue appMap apiName =
  fromMaybe (get "default") (Map.lookup apiName appMap)
  where
    get name =
      fromMaybe
        (error $ "Key " ++ T.unpack name ++ " not found in map " ++ show appMap)
        (Map.lookup name appMap)

getConfig :: [String] -> IO AppConfig
getConfig [] = readConfigFile "./config/default.json"
getConfig (f:_) = readConfigFile f

readConfigFile :: String -> IO AppConfig
readConfigFile f = do
  putStrLn $ "Config file " <> f
  json <- L.readFile f
  return $ fromJust (A.decode json)

runApp :: Bool -> Application -> AppConfig -> IO ()
runApp dev app conf = do
  let port = fromIntegral $ appPort conf
  run port $ withEnv dev logStdoutDev id app

withEnv :: Bool -> t -> t -> t
withEnv dev fd fp =
  if dev
    then fd
    else fp
