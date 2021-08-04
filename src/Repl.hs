{-# LANGUAGE OverloadedStrings #-}

module Repl where

import Control.Monad.Catch
import Data.ByteString.Char8 as B8
import Data.Aeson
import Network.HTTP.Simple
import System.Environment

getUserData :: MonadThrow m => m Request
getUserData = parseRequest "https://api.github.com/users/matto-mercury"

setUserAgent :: Request -> Request
setUserAgent bareReq =
  let (name, val) = ("User-Agent", "ghast-0.1.0.0") in
  addRequestHeader name val bareReq

httpValue :: Request -> IO (Response Value)
httpValue = httpJSON

authenticateWithBasic :: String -> String -> Request -> Request
authenticateWithBasic user pass =
  addRequestHeader "Authorization" $ B8.pack (user <> ":" <> pass)

getEnvironmentUserid :: IO (Maybe String)
getEnvironmentUserid = lookupEnv "GITHUB_USER"

getEnvironmentPassword :: IO (Maybe String)
getEnvironmentPassword = lookupEnv "GITHUB_KEY"

authorizedRequest :: Request -> IO Request
authorizedRequest req = do
  userid <- getEnvironmentUserid
  passwd <- getEnvironmentPassword

  case (userid, passwd) of
    (Just u, Just p) -> pure $ authenticateWithBasic u p $ setUserAgent req
    _ -> undefined