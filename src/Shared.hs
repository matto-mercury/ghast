module Shared where

-- this feels like a smell, but we'll roll with it as needed until I decide to
-- make a custom prelude or something like that

import Data.Aeson
import Data.Aeson.Casing (snakeCase)

aesonOptions :: Maybe String -> Options
aesonOptions ms = defaultOptions { fieldLabelModifier = snakeCase . drop len }
  where len = maybe 0 length ms