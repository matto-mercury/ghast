module Parsers.GhcErrors where

import Control.Applicative ((<|>))
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as P
import Data.Char (isAlpha)
import Data.Text (Text)

import Parsers.DateTime
import Parsers.Filepath

data GhcErrorHeader 
  = ErrorHeader LoggedFilepath
  | WarningHeader LoggedFilepath
  deriving stock (Show)

pErrorHeader :: Parser GhcErrorHeader 
pErrorHeader = do
  pDateTime
  file <- pHaskellPath
  string ": error:"
  endOfLine
  pure $ ErrorHeader file

pWarningHeader :: Parser GhcErrorHeader
pWarningHeader = do
  pDateTime
  file <- pHaskellPath
  string ": error:"
  skipSpace
  string "[-W" -- indicates a warning flag
  -- we don't actually care about the flag itself, probably, so I'm not going
  -- to bother parsing it out yet
  takeTill isEndOfLine
  endOfLine
  pure $ WarningHeader file

-- this will catch the error description text
pErrorText :: Parser Text
pErrorText = do
  pDateTime
  option "" (string "##[error]") -- first line seems to start with this
  skipSpace
  -- the next line that starts with | indicates the start of the code snippet,
  -- which we want to treat differently from the error text
  lookAhead $ notChar '|'
  text <- takeTill isEndOfLine
  endOfLine
  pure text

pErrorSnippetDelimiter :: Parser () -- just eat them
pErrorSnippetDelimiter = do
  pDateTime
  skipSpace
  string "|"
  takeTill isEndOfLine
  endOfLine
  pure ()

pErrorCodeSnippet :: Parser Text
pErrorCodeSnippet = do
  pDateTime
  decimal -- line number
  string " |"
  skipSpace
  code <- takeTill isEndOfLine
  endOfLine
  pure code

data GhcError = GhcError
  { errHeader :: GhcErrorHeader
  , errText :: [Text] -- this can probably be a NonEmpty
  , errSnippet :: Text
  }
  deriving stock (Show)

pGhcError :: Parser GhcError
pGhcError = do
  header <- pErrorHeader <|> pWarningHeader
  text <- many1 pErrorText
  pErrorSnippetDelimiter
  code <- pErrorCodeSnippet
  pErrorSnippetDelimiter
  pure GhcError 
    { errHeader = header
    , errText = text
    , errSnippet = code
    }