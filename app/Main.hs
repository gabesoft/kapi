module Main where

import qualified Handlers.Users.Xandar as XU
import Network.Wai
import Network.Wai.Handler.Warp

port = 8001

main :: IO ()
main = do
  putStrLn $ "Server started on port " ++ show port
  run port XU.app
