module UriFragmentSpec where

import Test.Hspec

import UriFragment

spec :: Spec
spec = do
  describe "Basic percent-encoding on rendered URI fragments" $ do
    it "does nothing when input doesn't need encoding" $
      render "hello" `shouldBe` "hello"

    it "percent-encodes spaces and ascii punctuation" $
      render "hi& there" `shouldBe` "hi%26%20there"

    it "percent-encodes multibyte characters" $
      render "hi günter" `shouldBe` "hi%20g%C3%BCnter"

  describe "Key-value parameters get rendered with a = between them" $ do
    it "does what it says on the tin" $
      render (Param "foo" "bar") `shouldBe` "foo=bar"

    it "plays nice with URI encoding" $
      render (Param "name" "günter") `shouldBe` "name=g%C3%BCnter"

  describe "Lists of key-value parameters get encoded properly" $ do
    it "successfully renders nothing for an empty list" $
      render ([] @Parameter) `shouldBe` ""

    it "renders a single parameter without fuss" $
      render [Param "key" "val"] `shouldBe` "?key=val"

    it "renders several parameters with intercalated &s" $
      render [Param "key" "val", Param "foo" "bar"] `shouldBe` "?key=val&foo=bar"