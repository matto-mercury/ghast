module Parsers.Filepath where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as P
import Data.Char (isAlpha)
import Data.Text (Text)

-- ghc compiler messages begin with a filepath that looks like
--  list/of/directories/sourcefile.hs:line:col
-- it is just as easy to parse these structured as not, or pretty close, so
-- that's what we're going to collect here. maybe we can use that to dedup
-- errors for specific files.

data LoggedFilepath = LoggedFilepath 
  { dirPath :: [Text]
  , file :: Text
  , line :: Int
  , col :: Int
  }
  deriving stock (Show)

pDirectory :: Parser Text
pDirectory = do
  dir <- P.takeWhile (\c -> isAlpha c || c == '-')
  char '/'
  pure dir

pDirectoryPath :: Parser [Text]
pDirectoryPath = many1 pDirectory

pHaskellFile :: Parser Text
pHaskellFile = do
  file <- P.takeWhile (/= '.')
  string ".hs"
  pure $ file <> ".hs"

pHaskellPath :: Parser LoggedFilepath
pHaskellPath = do
  path <- pDirectoryPath
  file <- pHaskellFile
  char ':'
  line <- decimal
  char ':'
  col <- decimal
  pure LoggedFilepath
    { dirPath = path
    , file = file
    , line = line
    , col = col 
    } 