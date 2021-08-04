module Main where

import Repl
import Data.Aeson
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = do
  baseReq <- getUserData
  authedReq <- authorizedRequest baseReq
  resp <- httpBS authedReq
  let body = getResponseBody resp
  B8.putStrLn body