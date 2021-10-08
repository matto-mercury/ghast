module Parsers.DateTimeSpec where

import Control.Monad.Catch
import Data.Attoparsec.Text
import Data.Text (Text)
import Test.Hspec

import Parsers.DateTime

spec :: Spec
spec = do
  describe "Parsing DateTimes and discarding them" $ do
    it "parses a datetime" $ do
      parseOnly pDateTime "2021-09-29T21:46:07.0508629Z some other shit"
      `shouldBe` Right ()
    it "also handles slashed dates" $ do
      parseOnly pDateTime "2021/09/29T21:46:07.0508629Z some other shit"
      `shouldBe` Right ()