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

-- first line in a set of test failures
pTestFailuresHeaderLine :: Parser ()
pTestFailuresHeaderLine = do
  pDateTime
  string "Failures:"
  endOfLine

-- last line in a set of test failures
pRerunSeedLine :: Parser Int
pRerunSeedLine = do
  pDateTime
  string "Randomized with seed "
  seed <- decimal
  endOfLine
  pure seed

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

newtype MatchPath = MatchPath [Text]
  deriving stock (Show, Eq)

pFailingSpecDetailsLine :: Parser Text
pFailingSpecDetailsLine = do
  pDateTime
  string "    " -- details are indented further than other test-fail lines
  skipSpace     -- there's probably more space
  deets <- takeTill isEndOfLine
  endOfLine
  pure deets

pFailingSpecMatchPath :: Parser MatchPath
pFailingSpecMatchPath = do
  pDateTime
  skipSpace
  string "To rerun use: --match \"/"
  matches <- sepBy1 (P.takeWhile (notInClass "/\n")) (char '/') -- takes trailing '"'
  endOfLine
  pure . MatchPath $ init matches

data TestFailure = TestFailure
  { filepath :: LoggedFilepath
  , testpath :: TestPath
  , details :: [Text]
  , matchpath :: MatchPath
  }
  deriving stock (Show, Eq)

pTestFailure :: Parser TestFailure
pTestFailure = do
  file <- pFailingSpecFileLine
  test <- pFailingSpecTestPath
  deets <- many1 pFailingSpecDetailsLine
  pOtherLogline -- empty line before match path
  match <- pFailingSpecMatchPath
  pOtherLogline
  pure $ TestFailure file test deets match

data TestSuiteFailures = TestSuiteFailures
  { failures :: [TestFailure]
  , seed :: Int
  }
  deriving stock (Show, Eq)

pTestSuiteFailures :: Parser TestSuiteFailures
pTestSuiteFailures = do
  pTestFailuresHeaderLine
  pOtherLogline
  fails <- many1 pTestFailure
  seed <- pRerunSeedLine
  pOtherLogline
  pure $ TestSuiteFailures fails seed