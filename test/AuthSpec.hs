module AuthSpec where

import Control.Monad.Catch
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Simple
import Repl
import Test.Hspec

spec :: Spec
spec = do
  describe "Basic auth header generator" $ do
    let userid = "user"
    let passwd = "pass"
    let authName = CI.mk $ B8.pack "Authorization"
    it "generates a body string that starts with 'Basic '" $ do
      body <- do
        bareReq <- spuriousRequest
        getRequestHeader authName $
            authenticateWithBasic userid passwd bareReq
      "Basic " `shouldBe` take 6 (B8.unpack body)

spuriousRequest :: MonadThrow m => m Request
spuriousRequest = parseRequest "https://foo.bar"