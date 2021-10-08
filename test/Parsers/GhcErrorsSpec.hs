module Parsers.GhcErrorsSpec where

import Control.Monad.Catch
import Data.Attoparsec.Text
import Data.Text (Text, intercalate)
import Test.Hspec
import Test.Hspec.Attoparsec

import Parsers.GhcErrors
import Parsers.Filepath

spec :: Spec
spec = do
  describe "Parsing the first line of a GHC error" $ do
    it "extracts a filepath and identifies errors" $ do
      let expected = ErrorHeader LoggedFilepath
            { dirPath = ["src"]
            , file = "Foo.hs"
            , line = 14
            , col = 15
            }
       in
        parseOnly pErrorHeader "2021-09-17T00:27:22.83Z src/Foo.hs:14:15: error:\n"
        `shouldBe`
        Right expected
    it "extracts a filepath and identifies warnings by [-W after the error" $ do
      let expected = WarningHeader LoggedFilepath
            { dirPath = ["src"]
            , file = "Foo.hs"
            , line = 5
            , col = 1
            }
       in
        parseOnly pWarningHeader "2021-09-17T00:27:22.84Z src/Foo.hs:5:1: error: [-Wunused-imports, -Werror=unused-imports]\n"
        `shouldBe`
        Right expected

  describe "Parsing the message out of a GHC error" $ do 
    it "returns lines stripped of datetimes and the ##[error] designation" $ do
      parseOnly pErrorText "2021-09-17T00:27:22.8474713Z ##[error]    The import of ‘Mercury.Banking.Types.Core’ is redundant\n"
      `shouldBe`
      Right "The import of ‘Mercury.Banking.Types.Core’ is redundant"
    it "also works when ##[error] isn't present" $ do
      parseOnly pErrorText "2021-09-17T00:27:22.8476135Z       except perhaps to import instances from ‘Mercury.Banking.Types.Core’\n"
      `shouldBe`
      Right "except perhaps to import instances from ‘Mercury.Banking.Types.Core’"

  describe "Parsing code snippets from GHC errors" $ do
    describe "Stripping out the parts we don't retain" $ do
      it "matches on the line above the snippet" $ do
        parseOnly pErrorSnippetDelimiter "2021-09-17T00:27:22.8477313Z   |\n"
        `shouldBe`
        Right ()
      it "matches on the underscores line" $ do
        parseOnly pErrorSnippetDelimiter "2021-09-17T00:27:22.8478476Z   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n"
        `shouldBe`
        Right ()

    describe "Extracting the line of the error" $ do
      it "pulls off the datetime and line number prefix" $ do
        parseOnly pErrorCodeSnippet "2021-09-17T00:27:22.8477837Z 5 | import Mercury.Banking.Types.Core (transactionAmountToDollar)\n"
        `shouldBe`
        Right "import Mercury.Banking.Types.Core (transactionAmountToDollar)"

  describe "Parsing whole GHC errors" $ do
    it "all works well together" $ do
      pGhcError `shouldSucceedOn` corpus

corpus :: Text
corpus = intercalate "\n"
  [ "2021-09-17T00:27:22.8473532Z src/Consumer/Jobs/SendDisputeEmails.hs:5:1: error: [-Wunused-imports, -Werror=unused-imports]"
  , "2021-09-17T00:27:22.8474713Z ##[error]    The import of ‘Mercury.Banking.Types.Core’ is redundant"
  , "2021-09-17T00:27:22.8476135Z       except perhaps to import instances from ‘Mercury.Banking.Types.Core’"
  , "2021-09-17T00:27:22.8476861Z     To import instances alone, use: import Mercury.Banking.Types.Core()"
  , "2021-09-17T00:27:22.8477313Z   |"
  , "2021-09-17T00:27:22.8477837Z 5 | import Mercury.Banking.Types.Core (transactionAmountToDollar)"
  , "2021-09-17T00:27:22.8478476Z   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n"
  ]