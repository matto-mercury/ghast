module Parsers.OtherLogline where

import Control.Applicative ((<|>))
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as P
import Data.Char (isAlpha)
import Data.Text (Text)

import Parsers.DateTime

pOtherLogline :: Parser () -- we just eat them
pOtherLogline = do
  pDateTime
  takeTill isEndOfLine
  endOfLine

pEmptyLogline :: Parser ()
pEmptyLogline = do
  pDateTime
  endOfLine