module Main where

import Repl
import Data.Aeson
import Data.CaseInsensitive
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = do
  baseReq <- getUserData
  authedReq <- authorizedRequest baseReq
  resp <- httpBS authedReq
  let body = getResponseBody resp
  B8.putStrLn body
  let headers = getResponseHeaders resp
  let headerStr = foldMap (\(k,v) -> (original k) <> v <> (B8.pack "\n")) headers
  B8.putStrLn headerStr