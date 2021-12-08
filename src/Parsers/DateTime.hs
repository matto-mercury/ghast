module Parsers.DateTime where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator (option)
import Data.Text (Text)

import LocalPrelude

-- We don't actually care (yet?) about DateTimes in ghast, but the build logs
-- all start with one. So a datetime parser serves two purposes:
-- - it matches the start of a line, more or less
-- - it lets us strip off that datetime before we show results to the user

pDateSep :: Parser ()
pDateSep = do
  char '-' <|> char '/'
  pure ()

pDate :: Parser ()
pDate = do
  count 4 digit -- year
  pDateSep
  count 2 digit -- month - at least for now, month and day are always two digits
  pDateSep
  count 2 digit -- day
  pure ()

pTime :: Parser ()
pTime = do
  count 2 digit -- hour
  char ':'
  count 2 digit -- minute
  char ':'
  double -- second (fractional)
  pure ()

-- do I want structured datetimes? not yet I don't
pDateTime :: Parser ()
pDateTime = do
  pDate
  char 'T'
  pTime
  char 'Z'
  -- if there is anything after the datetime, there will be at least one space
  -- between them. it's convenient to munch it here if it exists, but if it
  -- doesn't we don't want to fail the parse
  option ' ' (char ' ')
  pure ()