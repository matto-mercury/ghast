module Github.Jobs where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Text
import GHC.Generics
import Network.HTTP.Simple

import AppEnvironment
import Github
import Request
import Shared
import UriFragment

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

-- it looks as though /jobs/{jobid}/logs returns the logs for the job as a
-- string, as well as providing a link in the header
buildJobLogsRequest :: MonadThrow m => JobsProj -> AppT m Request
buildJobLogsRequest JobsProj {jpId} = do
  let logsReqUri = githubJobLogs jpId
  buildGithubRequest logsReqUri []

githubJobLogs :: Integer -> GitRemote -> Text
githubJobLogs jobId GitRemote {..} =
  mconcat 
    [ "/repos/"
    , owner
    , "/"
    , repo
    , "/actions/jobs/"
    , pack $ show jobId
    , "/logs"
    ]