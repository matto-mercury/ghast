module Repl where

import Control.Monad.Catch
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Base64 as B64
import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.List (intercalate)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Simple
import System.Environment

getUserData :: MonadThrow m => m Request
getUserData = parseRequest "https://api.github.com/users/matto-mercury"

setUserAgent :: Request -> Request
setUserAgent bareReq =
  let (name, val) = ("User-Agent", "ghast-0.1.0.0") in
  addRequestHeader name val bareReq

httpValue :: Request -> IO (Response Value)
httpValue = httpJSON

authenticateWithBasic :: String -> String -> Request -> Request
authenticateWithBasic user pass =
  addRequestHeader "Authorization" $ B8.pack "Basic " <> B64.encode (B8.pack $ user <> ":" <> pass)

getEnvironmentUserid :: IO (Maybe String)
getEnvironmentUserid = lookupEnv "GITHUB_USER"

getEnvironmentPassword :: IO (Maybe String)
getEnvironmentPassword = lookupEnv "GITHUB_KEY"

authorizedRequest :: Request -> IO Request
authorizedRequest req = do
  userid <- getEnvironmentUserid
  passwd <- getEnvironmentPassword

  case (userid, passwd) of
    (Just u, Just p) -> pure $ authenticateWithBasic u p $ setUserAgent req
    _ -> undefined

buildGithubRequest :: String -> [Parameter] -> IO Request
buildGithubRequest path params = do
  uaReq <- setUserAgent <$> parseRequest ("https://api.github.com" <> path <> writeParams params)
  authorizedRequest uaReq

githubListRuns = "/repos/MercuryTechnologies/mercury-web-backend/actions/runs"

data Parameter = Param String String

instance Show Parameter where
  -- probably want some URI-encoding here eventually
  show (Param k v) = k <> "=" <> v

writeParams :: [Parameter] -> String
writeParams [] = ""
writeParams ps = "?" <> pstr
  where pstr = intercalate "&" (map show ps)

perPage :: Integer -> Parameter
perPage n = Param "per_page" (show n)

aesonOptions :: Maybe String -> Options
aesonOptions ms = defaultOptions { fieldLabelModifier = snakeCase . drop len }
  where len = maybe 0 length ms

data UserDataProj = UserDataProj 
  { udpId :: Integer
  , udpName :: Text
  , udpNodeId :: Text
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON UserDataProj where
  parseJSON = genericParseJSON $ aesonOptions $ Just "udp"
  -- parseJSON = withObject "UserDataProj" $ \v -> UserDataProj
  --   <$> v .: "id"
  --   <*> v .: "name"
  --   <*> v .: "node_id"