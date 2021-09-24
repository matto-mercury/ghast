module Parsers.Filepath where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
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

pDirectoryPath :: Parser [Text]
pDirectoryPath = do
  let pLetter = notChar '/'
  let pDir = many1 pLetter
  pDir `sepBy1` char '/'