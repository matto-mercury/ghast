module Main where

import Data.Aeson
import Data.CaseInsensitive
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8

import AppEnvironment
import Repl

main :: IO ()
main = do
  jobs <- runAppEnv doWorkSon
  print jobs