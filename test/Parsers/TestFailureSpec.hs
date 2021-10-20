module Parsers.TestFailureSpec where

import Control.Monad.Catch
import Data.Attoparsec.Text
import qualified Data.Text as T (Text, intercalate)
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

    it "parses a match-path line" $ do
      parseOnly pFailingSpecMatchPath "2021-10-15T00:09:57.9409454Z   To rerun use: --match \"/Handler.Banking/Foo/bar with spaces/\"\n"
      `shouldBe`
      Right (MatchPath ["Handler.Banking", "Foo", "bar with spaces"])

    it "parses a failure details line" $ do
      parseOnly pFailingSpecDetailsLine "2021-10-15T00:09:57.9406623Z        Assertion: transaction has been disputed now\n"
      `shouldBe`
      Right "Assertion: transaction has been disputed now"

    it "parses a rerun seed line" $ do
      parseOnly pRerunSeedLine "2021-10-15T00:09:57.9410594Z Randomized with seed 1337012934\n"
      `shouldBe`
      Right 1337012934

  describe "parsing a single test failure" $ do
    let corpus = T.intercalate "\n" 
          [ "2021-10-15T00:09:57.9403958Z   test/Handler/Banking/SearchTransactionDisputedFlagSpec.hs:35:7:"
          , "2021-10-15T00:09:57.9405561Z   1) Handler.Banking.SearchTransactionDisputedFlag, Transaction disputed flags in transaction response, returns the correct value when there is and isn't a dispute"
          , "2021-10-15T00:09:57.9406623Z        Assertion: transaction has been disputed now"
          , "2021-10-15T00:09:57.9407199Z        First argument:  TransactionWasDisputedByUser"
          , "2021-10-15T00:09:57.9407909Z        Second argument: TransactionWasNotDisputed"
          , "2021-10-15T00:09:57.9408262Z"
          , "2021-10-15T00:09:57.9409454Z   To rerun use: --match \"/Handler.Banking.SearchTransactionDisputedFlag/Transaction disputed flags in transaction response/returns the correct value when there is and isn't a dispute/\""
          , "2021-10-15T00:09:57.9410318Z\n"
          ]

    let expected = TestFailure
          { filepath = LoggedFilepath ["test", "Handler", "Banking"] "SearchTransactionDisputedFlagSpec.hs" 35 7
          , testpath = TestPath 1 "Handler.Banking.SearchTransactionDisputedFlag, Transaction disputed flags in transaction response, returns the correct value when there is and isn't a dispute"
          , details = [ "Assertion: transaction has been disputed now"
                      , "First argument:  TransactionWasDisputedByUser"
                      , "Second argument: TransactionWasNotDisputed"
                      ]
          , matchpath = MatchPath [ "Handler.Banking.SearchTransactionDisputedFlag"
                                   , "Transaction disputed flags in transaction response"
                                   , "returns the correct value when there is and isn't a dispute"
                                   ]
          }

    it "parses a single test failure" $ do
      parseOnly pTestFailure corpus
      `shouldBe`
      Right expected

  describe "parsing multiple test failures" $ do
    let corpus = T.intercalate "\n" 
          [ "2021-10-15T00:09:57.9403181Z Failures:"
          , "2021-10-15T00:09:57.9403364Z"
          , "2021-10-15T00:09:57.9403958Z   test/Handler/Banking/SearchTransactionDisputedFlagSpec.hs:35:7:"
          , "2021-10-15T00:09:57.9405561Z   1) Handler.Banking.SearchTransactionDisputedFlag, Transaction disputed flags in transaction response, returns the correct value when there is and isn't a dispute"
          , "2021-10-15T00:09:57.9406623Z        Assertion: transaction has been disputed now"
          , "2021-10-15T00:09:57.9407199Z        First argument:  TransactionWasDisputedByUser"
          , "2021-10-15T00:09:57.9407909Z        Second argument: TransactionWasNotDisputed"
          , "2021-10-15T00:09:57.9408262Z"
          , "2021-10-15T00:09:57.9409454Z   To rerun use: --match \"/Handler.Banking.SearchTransactionDisputedFlag/Transaction disputed flags in transaction response/returns the correct value when there is and isn't a dispute/\""
          , "2021-10-15T00:09:57.9410318Z"
          , "2021-10-15T00:09:57.9403958Z   test/Handler/Banking/SearchTransactionDisputedFlagSpec.hs:35:7:"
          , "2021-10-15T00:09:57.9405561Z   2) Handler.Banking.SearchTransactionDisputedFlag, Transaction disputed flags in transaction response, returns the correct value when there is and isn't a dispute"
          , "2021-10-15T00:09:57.9406623Z        Assertion: transaction has been disputed now"
          , "2021-10-15T00:09:57.9407199Z        First argument:  TransactionWasDisputedByUser"
          , "2021-10-15T00:09:57.9407909Z        Second argument: TransactionWasNotDisputed"
          , "2021-10-15T00:09:57.9408262Z"
          , "2021-10-15T00:09:57.9409454Z   To rerun use: --match \"/Handler.Banking.SearchTransactionDisputedFlag/Transaction disputed flags in transaction response/returns the correct value when there is and isn't a dispute/\""
          , "2021-10-15T00:09:57.9410318Z"
          , "2021-10-15T00:09:57.9410594Z Randomized with seed 1337012934"
          , "2021-10-15T00:09:57.9410811Z\n"
          ]

    it "should pull out both failures and the seed" $ do
      let (Right testFailures) = parseOnly pTestSuiteFailures corpus
      length (failures testFailures) `shouldBe` 2
      seed testFailures `shouldBe` 1337012934