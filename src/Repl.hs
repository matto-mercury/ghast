{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import qualified Network.URI.Encode as UE
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
    (Just u, Just p) -> pure $ authenticateWithBasic u p req
    _ -> undefined

buildGithubRequest :: String -> [Parameter] -> IO Request
buildGithubRequest path params = do
  uaReq <- setUserAgent <$> parseRequest ("https://api.github.com" <> path <> render params)
  authorizedRequest uaReq

githubListRuns = "/repos/MercuryTechnologies/mercury-web-backend/actions/runs"

data Parameter = Param String String
  deriving stock (Show, Eq)

class UriFragment s where
  render :: s -> String

instance UriFragment String where
  -- probably want some URI-encoding here eventually
  render = UE.encode

instance UriFragment Parameter where
  render (Param k v) = render k <> "=" <> render v

instance UriFragment [Parameter] where
  render [] = ""
  render ps = "?" <> pstr
    where pstr = intercalate "&" (map render ps)

perPage :: Integer -> Parameter
perPage n = Param "per_page" (show n)

branch :: String -> Parameter
branch = Param "branch"

aesonOptions :: Maybe String -> Options
aesonOptions ms = defaultOptions { fieldLabelModifier = snakeCase . drop len }
  where len = maybe 0 length ms

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

data ListRuns = ListRuns
  { lrTotalCount :: Integer
  , lrWorkflowRuns :: [CommitRunProj]
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON ListRuns where
  parseJSON = genericParseJSON $ aesonOptions $ Just "lr"

data CommitRunProj = CommitRunProj
  { crpId :: Integer
  , crpHeadBranch :: String
  , crpStatus :: String
  , crpConclusion :: String
  , crpJobsUrl :: String
  , crpLogsUrl :: String -- sends back a zipfile, apparently, probably not what I want
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON CommitRunProj where
  parseJSON = genericParseJSON $ aesonOptions $ Just "crp"

runJobsRequest :: CommitRunProj -> [Parameter] -> IO Request
runJobsRequest CommitRunProj {crpJobsUrl} params = do -- NamedFieldPuns instead of {..}
  uaReq <- setUserAgent <$> parseRequest (crpJobsUrl <> render params)
  authorizedRequest uaReq

runLogsRequest :: CommitRunProj -> [Parameter] -> IO Request
runLogsRequest CommitRunProj {crpLogsUrl} params = do
  uaReq <- setUserAgent <$> parseRequest (crpLogsUrl <> render params)
  authorizedRequest uaReq

data ListJobs = ListJobs
  { ljTotalCount :: Integer
  , ljJobs :: [JobsProj]
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON ListJobs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "lj"

data JobsProj = JobsProj
  { jpId :: Integer
  , jpRunId :: Integer
  , jpStatus :: JobStatus
  , jpConclusion :: JobConclusion
  , jpStartedAt :: String
  , jpCompletedAt :: String
  , jpName :: String
  , jpSteps :: [JobSteps]
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON JobsProj where
  parseJSON = genericParseJSON $ aesonOptions $ Just "jp"

data JobSteps = JobSteps
  { jsName :: String
  , jsStatus :: JobStatus
  , jsConclusion :: JobConclusion
  , jsNumber :: Integer
  , jsStartedAt :: String
  , jsCompletedAt :: String
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON JobSteps where
  parseJSON = genericParseJSON $ aesonOptions $ Just "js"

-- these aren't really documented anywhere that I can find in the API guide,
-- but I'm stealing these enum values from the gh cli's source:
--  pkg/cmd/run/shared/shared.go
data JobStatus
  = Queued
  | Completed
  | InProgress
  | Requested
  | Waiting
  deriving stock (Generic, Show, Eq)

instance FromJSON JobStatus where
  parseJSON = genericParseJSON $ defaultOptions {constructorTagModifier = snakeCase}

data JobConclusion
  = Success
  | Failure
  | Cancelled
  | Skipped
  | ActionRequired
  | Neutral
  | Stale
  | StartupFailure
  | TimedOut
  deriving stock (Generic, Show, Eq)

instance FromJSON JobConclusion where
  parseJSON = genericParseJSON $ defaultOptions {constructorTagModifier = snakeCase}


-- jeez
doWorkSon :: String -> IO [JobsProj]
doWorkSon br = do
  runReq <- buildGithubRequest githubListRuns [perPage 1, branch br]
  runResp <- httpJSON @_ @ListRuns runReq

  let runs = lrWorkflowRuns $ getResponseBody runResp

  jobsReq <- runJobsRequest (head runs) []
  jobsResp <- httpJSON @_ @ListJobs jobsReq

  let jobs = ljJobs $ getResponseBody jobsResp

  pure jobs 