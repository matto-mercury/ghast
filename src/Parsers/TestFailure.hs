module Parsers.TestFailure where

import Control.Applicative ((<|>))
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as P
import Data.Char (isAlpha)
import Data.Maybe (catMaybes)
import Data.Text (Text)

import Parsers.DateTime
import Parsers.Filepath
import Parsers.OtherLogline

pTestFailuresHeaderLine :: Parser ()
pTestFailuresHeaderLine = do
  pDateTime
  string "Failures:"
  endOfLine

pFailingSpecFileLine :: Parser LoggedFilepath
pFailingSpecFileLine = do
  pDateTime
  skipSpace
  path <- pHaskellPath
  char ':'
  endOfLine
  pure path

data TestPath = TestPath
  { failureNum :: Int
  , testPath :: Text
  }
  deriving stock (Show, Eq)

pFailingSpecTestPath :: Parser TestPath
pFailingSpecTestPath = do
  pDateTime
  skipSpace
  failureNum <- decimal
  char ')'
  skipSpace
  path <- takeTill isEndOfLine
  endOfLine
  pure $ TestPath failureNum path