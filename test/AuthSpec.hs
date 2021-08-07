module AuthSpec where

import Control.Monad.Catch
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Simple
import Network.HTTP.Types.Header
import Repl
import Test.Hspec

spec :: Spec
spec = do
  describe "Basic auth header generator" $ do
    it "generates a value string that starts with 'Basic '" $ do
      value <- do head . getRequestHeader authName
       . authenticateWithBasic userid passwd
       <$> spuriousRequest
      "Basic " `shouldBe` take 6 (B8.unpack value)

spuriousRequest :: MonadThrow m => m Request
spuriousRequest = parseRequest "https://foo.bar"

userid :: String
userid = "user"

passwd :: String
passwd = "pass"

authName :: HeaderName
authName = CI.mk $ B8.pack "Authorization"