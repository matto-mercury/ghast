module Args where

import Data.Text
import Options.Generic

-- todo: help text
data Args = Args 
  { branch :: Maybe Text
  , rawlogs :: Bool
  , thisrun :: Maybe Int
  }
  deriving (Generic, Show)

instance ParseRecord Args