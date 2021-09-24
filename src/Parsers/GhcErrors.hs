module Parsers.GhcErrors where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as P
import Data.Char (isAlpha)
import Data.Text (Text)

import Parsers.DateTime
import Parsers.Filepath

newtype ErrorHeader = ErrorHeader { file :: LoggedFilepath }
  deriving stock (Show)

pErrorHeader :: Parser ErrorHeader 
pErrorHeader = do
  pDateTime
  file <- pHaskellPath
  string ": error:"
  endOfLine
  pure ErrorHeader { file = file }

-- this will catch the error description text
pErrorText :: Parser Text
pErrorText = do
  pDateTime
  option "" (string "##[error]") -- first line seems to start with this
  skipSpace
  text <- takeTill (\c -> (c == '|') || isEndOfLine c)
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
  { errHeader :: ErrorHeader
  , errText :: [Text]
  , errSnippet :: Text
  }
  deriving stock (Show)

pGhcError :: Parser GhcError
pGhcError = do
  header <- pErrorHeader
  text <- many1 pErrorText
  pErrorSnippetDelimiter
  code <- pErrorCodeSnippet
  pErrorSnippetDelimiter
  pure GhcError 
    { errHeader = header
    , errText = text
    , errSnippet = code
    }