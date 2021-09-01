module AuthSpec where

import Control.Monad.Catch
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import Data.Text (Text (..))
import Network.HTTP.Simple
import Network.HTTP.Types.Header
import qualified Repl as R
import Test.Hspec

spec :: Spec
spec = do
  describe "Basic auth header generator" $ do
    it "generates a value string that starts with 'Basic '" $ do
      value <- do head . getRequestHeader authName
       . R.authenticateWithBasic userid passwd
       <$> spuriousRequest
      take 6 (B8.unpack value) `shouldBe` "Basic "

    it "appends base-64 encoding of 'user:pass'" $ do
      value <- do head . getRequestHeader authName
        . R.authenticateWithBasic userid passwd
        <$> spuriousRequest
      drop 6 (B8.unpack value) `shouldBe` "dXNlcjpwYXNz"

spuriousRequest :: MonadThrow m => m Request
spuriousRequest = parseRequest "https://foo.bar"

userid :: Text
userid = "user"

passwd :: Text
passwd = "pass"

authName :: HeaderName
authName = CI.mk $ B8.pack "Authorization"