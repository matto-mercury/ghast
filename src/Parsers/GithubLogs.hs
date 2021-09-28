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
import Parsers.OtherLogline

pmGhcError :: Parser (Maybe GhcError)
pmGhcError = Just <$> pGhcError

pmOtherLine :: Parser (Maybe GhcError)
pmOtherLine = do
  pOtherLogline
  pure Nothing

pmLoggedItems :: Parser [Maybe GhcError]
pmLoggedItems = many1 $ pmGhcError <|> pmOtherLine

-- this return type will have to grow eventually
parseGithubJobLogs :: Text -> Either String [GhcError]
parseGithubJobLogs logs = catMaybes <$> parseOnly pmLoggedItems logs

-- not sure if this belongs here but whatever
-- eventually this'll take a verbosity argument
prettify :: Either String [GhcError] -> String
prettify = \case
  Left s -> "shit, failed to parse (" ++ s ++ "), try running with --rawlogs"
  Right errors -> intercalate "\n" $ renderGhcError <$> errors

renderGhcError :: GhcError -> String
renderGhcError GhcError {..} = intercalate "\n"
  [ renderHeader errHeader
  , "  " ++ unpack (head errText)
  , "  " ++ renderSnippetWith (pathOf errHeader) errSnippet
  ]

-- this is awkward and is telling me my GhcErrorHeader type sucks
pathOf :: GhcErrorHeader -> LoggedFilepath
pathOf = \case
  ErrorHeader path -> path
  WarningHeader path -> path

renderHeader :: GhcErrorHeader -> String
renderHeader = \case
  ErrorHeader path -> "Err: " ++ renderPath path
  WarningHeader path -> "Warn: " ++ renderPath path

renderPath :: LoggedFilepath -> String
renderPath LoggedFilepath {..} = intercalate "/" strings
  where strings = (unpack <$> dirPath) ++ [unpack file]

renderSnippetWith :: LoggedFilepath -> Text -> String
renderSnippetWith LoggedFilepath {line} snippet =
  show line ++ ": " ++ unpack snippet