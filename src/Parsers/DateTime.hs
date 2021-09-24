module Parsers.DateTime where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Text (Text)

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
  count 2 digit -- second
  pure ()

-- do I want structured datetimes? not yet I don't
pDateTime :: Parser ()
pDateTime = do
  pDate
  char 'T'
  pTime
  char 'Z'
  char ' '
  pure ()