{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Repl where

import Control.Monad.Catch
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Base64 as B64
import qualified Data.Aeson as J
import Data.Aeson.Casing (snakeCase)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Simple
import System.Environment

import UriFragment

getUserData :: MonadThrow m => m Request
getUserData = parseRequest "https://api.github.com/users/matto-mercury"

setUserAgent :: Request -> Request
setUserAgent bareReq =
  let (name, val) = ("User-Agent", "ghast-0.1.0.0") in
  addRequestHeader name val bareReq

authenticateWithBasic :: String -> String -> Request -> Request
authenticateWithBasic user pass =
  addRequestHeader "Authorization" $ B8.pack "Basic " <> B64.encode (B8.pack $ user <> ":" <> pass)

authorizedRequest :: Monad m => Request -> AppT m Request
authorizedRequest req = do
  env <- ask

  pure $ authenticateWithBasic (userId env) (passwd env) req

buildGithubRequest :: MonadThrow m => String -> [Parameter] -> AppT m Request
buildGithubRequest path params = do
  uaReq <- setUserAgent <$> parseRequest ("https://api.github.com" <> path <> render params)
  authorizedRequest uaReq

githubListRuns = "/repos/MercuryTechnologies/mercury-web-backend/actions/runs"
perPage :: Integer -> Parameter
perPage n = Param "per_page" (show n)

branch :: String -> Parameter
branch = Param "branch"

aesonOptions :: Maybe String -> J.Options
aesonOptions ms = J.defaultOptions { J.fieldLabelModifier = snakeCase . drop len }
  where len = maybe 0 length ms

data UserDataProj = UserDataProj 
  { udpId :: Integer
  , udpName :: String
  , udpNodeId :: String
  }
  deriving stock (Generic, Show, Eq)

instance J.FromJSON UserDataProj where
  parseJSON = J.genericParseJSON $ aesonOptions $ Just "udp"
  -- parseJSON = withObject "UserDataProj" $ \v -> UserDataProj
  --   <$> v .: "id"
  --   <*> v .: "name"
  --   <*> v .: "node_id"

data ListRuns = ListRuns
  { lrTotalCount :: Integer
  , lrWorkflowRuns :: [CommitRunProj]
  }
  deriving stock (Generic, Show, Eq)

instance J.FromJSON ListRuns where
  parseJSON = J.genericParseJSON $ aesonOptions $ Just "lr"

data CommitRunProj = CommitRunProj
  { crpId :: Integer
  , crpHeadBranch :: String
  , crpStatus :: String
  , crpConclusion :: String
  , crpJobsUrl :: String
  , crpLogsUrl :: String -- sends back a zipfile, apparently, probably not what I want
  }
  deriving stock (Generic, Show, Eq)

instance J.FromJSON CommitRunProj where
  parseJSON = J.genericParseJSON $ aesonOptions $ Just "crp"

runJobsRequest :: MonadThrow m => CommitRunProj -> [Parameter] -> AppT m Request
runJobsRequest CommitRunProj {crpJobsUrl} params = do -- NamedFieldPuns instead of {..}
  uaReq <- setUserAgent <$> parseRequest (crpJobsUrl <> render params)
  authorizedRequest uaReq

runLogsRequest :: MonadThrow m => CommitRunProj -> [Parameter] -> AppT m Request
runLogsRequest CommitRunProj {crpLogsUrl} params = do
  uaReq <- setUserAgent <$> parseRequest (crpLogsUrl <> render params)
  authorizedRequest uaReq

data ListJobs = ListJobs
  { ljTotalCount :: Integer
  , ljJobs :: [JobsProj]
  }
  deriving stock (Generic, Show, Eq)

instance J.FromJSON ListJobs where
  parseJSON = J.genericParseJSON $ aesonOptions $ Just "lj"

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

instance J.FromJSON JobsProj where
  parseJSON = J.genericParseJSON $ aesonOptions $ Just "jp"

data JobSteps = JobSteps
  { jsName :: String
  , jsStatus :: JobStatus
  , jsConclusion :: JobConclusion
  , jsNumber :: Integer
  , jsStartedAt :: String
  , jsCompletedAt :: String
  }
  deriving stock (Generic, Show, Eq)

instance J.FromJSON JobSteps where
  parseJSON = J.genericParseJSON $ aesonOptions $ Just "js"

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

instance J.FromJSON JobStatus where
  parseJSON = J.genericParseJSON $ J.defaultOptions {J.constructorTagModifier = snakeCase}

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

instance J.FromJSON JobConclusion where
  parseJSON = J.genericParseJSON $ J.defaultOptions {J.constructorTagModifier = snakeCase}

-- jeez
doWorkSon :: (MonadThrow m, MonadIO m) => String -> AppT m [JobsProj]
doWorkSon br = do
  runReq <- buildGithubRequest githubListRuns [perPage 1, branch br]
  runResp <- httpJSON @_ @ListRuns runReq

  let runs = lrWorkflowRuns $ getResponseBody runResp

  jobsReq <- runJobsRequest (head runs) []
  jobsResp <- httpJSON @_ @ListJobs jobsReq

  let jobs = ljJobs $ getResponseBody jobsResp

  pure $ filter (\j -> (jpStatus j == Completed) && (jpConclusion j /= Success)) jobs 

data Env = Env
  { userId :: String
  , passwd :: String
  }
  deriving Show

newtype AppT m a = AppT { runAppT :: ReaderT Env m a }
  deriving newtype (MonadReader Env, Monad, Applicative, Functor, MonadIO, MonadThrow)

getEnvironmentUserid :: IO (Maybe String)
getEnvironmentUserid = lookupEnv "GITHUB_USER"

getEnvironmentPassword :: IO (Maybe String)
getEnvironmentPassword = lookupEnv "GITHUB_KEY"

readEnvCreds :: IO Env
readEnvCreds = do
  mUserid <- getEnvironmentUserid
  mPasswd <- getEnvironmentPassword

  case (mUserid, mPasswd) of
    (Just u, Just p) -> pure Env { userId = u , passwd = p }
    (_, _) -> error "oops"

runAppEnv :: AppT IO a -> IO a
runAppEnv app = do
  env <- readEnvCreds
  runReaderT (runAppT app) env
  -- case app of
  --   AppT a -> case a of
  --     ReaderT ema -> ema env

