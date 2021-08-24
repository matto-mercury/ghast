module Main where

import Repl
import Data.Aeson
import Data.CaseInsensitive
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = do
  jobs <- runAppEnv (doWorkSon "matto/rul-79")
  print jobs