module Github where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson
import Data.Text
import Network.HTTP.Simple
import GHC.Generics

import AppEnvironment
import Request
import Shared
import UriFragment

perPage :: Integer -> Parameter
perPage n = Param "per_page" (pack $ show n)

branch :: Text -> Parameter
branch = Param "branch"

buildGithubRequest :: MonadThrow m => (GitRemote -> Text) -> [Parameter] -> AppT m Request
buildGithubRequest pathFunc params = do
  (u, p) <- asks getCreds
  remote <- asks gitRemote
  let path = pathFunc remote
  uaReq <- setUserAgent <$> parseRequest (unpack $ "https://api.github.com" <> path <> render params)
  pure $ authenticateWithBasic u p uaReq

hardPath :: Text -> GitRemote -> Text
hardPath = const

-- not really used any more, if it gets load-bearing again it'll get its own
-- module. as it stands I'm just keeping it around for historical interest

data UserDataProj = UserDataProj 
  { udpId :: Integer
  , udpName :: String
  , udpNodeId :: String
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON UserDataProj where
  parseJSON = genericParseJSON $ aesonOptions $ Just "udp"
  -- parseJSON = withObject "UserDataProj" $ \v -> UserDataProj
  --   <$> v .: "id"
  --   <*> v .: "name"
  --   <*> v .: "node_id"