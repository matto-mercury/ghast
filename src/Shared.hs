module Shared where

-- this feels like a smell, but we'll roll with it as needed until I decide to
-- make a custom prelude or something like that

import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Text (Text, pack)

aesonOptions :: Maybe String -> Options
aesonOptions ms = defaultOptions { fieldLabelModifier = snakeCase . drop len }
  where len = maybe 0 length ms

tshow :: Show a => a -> Text
tshow = pack . show

-- this comes in a lot of modules but I feel like importing one is overkill?
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither foo = \case
  Nothing -> Left foo
  Just bar -> Right bar