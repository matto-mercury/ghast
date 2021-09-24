module Parsers.GhcErrorHeader where

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