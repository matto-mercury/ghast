module Parsers.TestFailureSpec where

import Control.Monad.Catch
import Data.Attoparsec.Text
import Data.Text (Text, intercalate)
import Test.Hspec
import Test.Hspec.Attoparsec

import Parsers.Filepath
import Parsers.TestFailure

spec :: Spec
spec = do
  describe "parsing subsets of a test failure" $ do
    it "parses a filepath with some whitespace in front of it" $ do
      parseOnly pFailingSpecFileLine "2021-10-15T00:09:57.9403958Z   test/Handler/Banking/SearchTransactionDisputedFlagSpec.hs:35:7:\n"
      `shouldBe`
      Right LoggedFilepath
        { dirPath = ["test", "Handler", "Banking"]
        , file = "SearchTransactionDisputedFlagSpec.hs"
        , line = 35
        , col = 7
        }
    it "parses a test failure header" $ do
      parseOnly pFailingSpecTestPath "2021-10-07T23:07:05.3277417Z   1) Mercury.Typescript.Generate, TypeScript generation, goldenTestTypescriptTypes\n"
      `shouldBe`
      Right TestPath
        { failureNum = 1
        , testPath = "Mercury.Typescript.Generate, TypeScript generation, goldenTestTypescriptTypes"
        }
