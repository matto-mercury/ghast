{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Parsers.GithubLogs where

import Control.Applicative ((<|>))
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as P
import Data.Char (isAlpha)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)

import Parsers.Filepath
import Parsers.GhcErrors
import Parsers.MigrationMismatch
import Parsers.OtherLogline
import Parsers.TestFailure

data BuildError
  = DiscardedLine
  | CompilerError GhcError
  | FailedTest TestSuiteFailures
  | MigrationError MigrationMismatch

pBuildError :: Parser BuildError
pBuildError = CompilerError <$> pGhcError

pMigrationError :: Parser BuildError
pMigrationError = MigrationError <$> pMigrationMismatch

pFailedTest :: Parser BuildError
pFailedTest = FailedTest <$> pTestSuiteFailures

pDiscardedLine :: Parser BuildError
pDiscardedLine = do
  pOtherLogline
  pure DiscardedLine

pLoggedItems :: Parser [BuildError]
pLoggedItems = many1 $ pBuildError <|> pMigrationError <|> pFailedTest <|> pDiscardedLine

parseGithubJobLogs :: Text -> Either String [BuildError]
parseGithubJobLogs logs = catErrors <$> parseOnly pLoggedItems logs

catErrors :: [BuildError] -> [BuildError]
catErrors [] = []
catErrors (e:es) = case e of
  DiscardedLine -> catErrors es
  a -> a : catErrors es

-- not sure if this belongs here but whatever
-- eventually this'll take a verbosity argument
prettify :: Either String [BuildError] -> String
prettify = \case
  Left s -> "shit, failed to parse (" ++ s ++ "), try running with --rawlogs"
  Right errors -> renderDefaultBuildErrors errors

renderDefaultBuildErrors :: [BuildError] -> String
renderDefaultBuildErrors errors = intercalate "\n" $ renderBuildError <$> errors

renderBuildError :: BuildError -> String
renderBuildError = \case
  DiscardedLine -> "can't happen"
  CompilerError e -> renderGhcError e
  MigrationError e -> renderMigrationError e
  FailedTest t -> renderFailedTest t

renderGhcError :: GhcError -> String
renderGhcError GhcError {..} = intercalate "\n"
  [ renderHeader errHeader
  , "  " ++ unpack (head errText)
  , "  " ++ renderSnippetWith (pathOf errHeader) errSnippet
  ]

renderMigrationError :: MigrationMismatch -> String
renderMigrationError MigrationMismatch {..} = intercalate "\n" $
  "Suggested migrations:" : (unpack <$> migrations)

-- this is awkward and is telling me my GhcErrorHeader type sucks
pathOf :: GhcErrorHeader -> LoggedFilepath
pathOf = \case
  ErrorHeader path -> path
  WarningHeader path -> path

renderHeader :: GhcErrorHeader -> String
renderHeader = \case
  ErrorHeader path -> "Err: " ++ renderPath path
  WarningHeader path -> "Warn: " ++ renderPath path

renderSnippetWith :: LoggedFilepath -> Text -> String
renderSnippetWith LoggedFilepath {line} snippet =
  show line ++ ": " ++ unpack snippet