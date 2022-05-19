module Args where

import Options.Generic

import LocalPrelude

data RenderVerbosity = RenderQuiet | RenderTerse | RenderNormal | RenderVerbose
  deriving (Generic, Show, Read)

instance ParseField RenderVerbosity

-- todo: help text
data Args = Args 
  { branch :: Maybe Text
  , runname :: Maybe Text
  , rawlogs :: Bool
  , thisrun :: Maybe Int
  , verbosity :: Maybe RenderVerbosity
  }
  deriving (Generic, Show)

instance ParseRecord Args