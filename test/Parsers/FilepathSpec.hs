module Parsers.FilepathSpec where

import Control.Monad.Catch
import Data.Attoparsec.Text
import Data.Text (Text)
import Test.Hspec

import Parsers.Filepath

spec :: Spec
spec = do
  describe "Parsing a filepath from a GHC error message" $ do
    it "pulls out subdirs, filename, line, and column" $ do
      parseOnly pHaskellPath "src/Mailer/Disputes/SendCustomerAcknowledgement.hs:14:15: error:"
      `shouldBe`
      Right LoggedFilepath
        { dirPath = ["src", "Mailer", "Disputes"]
        , file = "SendCustomerAcknowledgement.hs"
        , line = 14
        , col = 15
        }