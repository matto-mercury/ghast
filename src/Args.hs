module Args where

import Data.Text
import Options.Generic

data RenderVerbosity = RenderQuiet | RenderTerse | RenderNormal | RenderVerbose
  deriving (Generic, Show, Read)

instance ParseField RenderVerbosity

-- todo: help text
data Args = Args 
  { branch :: Maybe Text
  , rawlogs :: Bool
  , thisrun :: Maybe Int
  , verbosity :: Maybe RenderVerbosity
  }
  deriving (Generic, Show)

instance ParseRecord Args