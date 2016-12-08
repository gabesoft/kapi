module Main where

import qualified Handlers.Users.Xandar as XU
import Network.Wai
import Network.Wai.Handler.Warp

main :: IO ()
main = run 8001 XU.app
