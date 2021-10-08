module Request where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B8
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple

import AppEnvironment
import UriFragment

setUserAgent :: Request -> Request
setUserAgent bareReq =
  let (name, val) = ("User-Agent", "ghast-0.3.0.0") in
  addRequestHeader name val bareReq

authenticateWithBasic :: Text -> Text -> Request -> Request
authenticateWithBasic user pass =
  let u = encodeUtf8 user
      p = encodeUtf8 pass
  in
  addRequestHeader "Authorization" $ B8.pack "Basic " <> B64.encode (u <> ":" <> p)

-- response is a phantom type - type-level construct only
-- on the left side but not the right (value-level) side
newtype TypedRequest response =
  TypedRequest { unTypedRequest :: Request }

runTypedRequest :: MonadIO io => FromJSON a => TypedRequest a -> io (Response a)
runTypedRequest req = do
  httpJSON (unTypedRequest req)

runTypedRequestM :: MonadIO m => FromJSON a => AppT m (TypedRequest a) -> AppT m (Response a)
runTypedRequestM req = runTypedRequest =<< req

