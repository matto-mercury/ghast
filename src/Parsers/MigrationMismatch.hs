module Parsers.MigrationMismatch where

import Control.Applicative ((<|>))
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as P
import Data.Char (isAlpha)
import Data.Maybe (catMaybes)
import Data.Text (Text)

import Parsers.DateTime

newtype MigrationMismatch = MigrationMismatch 
  { migrations :: [Text]
  }
  deriving stock (Show)

pMigrationMismatch :: Parser MigrationMismatch
pMigrationMismatch = do
  -- this keys off of the last line of the error text in mwb/Application.hs,
  -- compareMigrationsToModels. if that changes this'll have to, too
  --
  -- there are three lines of static output in compareMigrationsToModels, but
  -- we're only looking for the last one
  pIntentionalMismatchLine
  lines <- many1 pMigrationLine
  pFixYourMigrationsLine
  pure MigrationMismatch { migrations = lines }

pIntentionalMismatchLine :: Parser ()
pIntentionalMismatchLine = do
  pDateTime
  string "If you have an intentional mismatch"
  takeTill isEndOfLine -- there's more, but we don't care, we got what we wanted
  endOfLine
  pure ()

pMigrationLine :: Parser Text
pMigrationLine = do
  -- ultimately this feeds off of the output of `migrateModels` from Persist.TH,
  -- which is eventually just SQL, so we could parse it to a more structured
  -- form. that sounds like a ton of effort and I'm not sure it'd be worth the
  -- trouble
  pDateTime
  lookAhead $ notChar 't' -- matching first char of "test:"
  sql <- takeTill isEndOfLine
  endOfLine
  pure sql

pFixYourMigrationsLine :: Parser ()
pFixYourMigrationsLine = do
  pDateTime
  string "test: MigrationMismatch" -- again this is a prefix
  takeTill isEndOfLine
  endOfLine
  pure ()