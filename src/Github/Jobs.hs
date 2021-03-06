module Github.Jobs where

import Control.Monad.Catch
import Control.Monad.Reader
import qualified Data.Aeson as JS
import Data.Aeson.Casing (snakeCase)
import Data.Text (pack)
import Data.Time.Clock
import GHC.Generics
import Network.HTTP.Simple

import LocalPrelude

import AppEnvironment
import Github
import Request
import Shared
import UriFragment

data ListJobs = ListJobs
  { ljTotalCount :: Int
  , ljJobs :: [JobsProj]
  }
  deriving stock (Generic, Show, Eq)

instance JS.FromJSON ListJobs where
  parseJSON = JS.genericParseJSON $ aesonOptions $ Just "lj"

data JobsProj = JobsProj
  { jpId :: Int
  , jpRunId :: Int
  , jpStatus :: JobStatus
  , jpConclusion :: JobConclusion
  , jpStartedAt :: UTCTime
  , jpCompletedAt :: UTCTime
  , jpName :: Text
  , jpSteps :: [JobStep]
  }
  deriving stock (Generic, Show, Eq)

instance JS.FromJSON JobsProj where
  parseJSON = JS.genericParseJSON $ aesonOptions $ Just "jp"

data JobStep = JobStep
  { jsName :: Text
  , jsStatus :: JobStatus
  , jsConclusion :: JobConclusion
  , jsNumber :: Int
  , jsStartedAt :: UTCTime
  , jsCompletedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)

instance JS.FromJSON JobStep where
  parseJSON = JS.genericParseJSON $ aesonOptions $ Just "js"

-- these aren't really documented anywhere that I can find in the API guide,
-- but I'm stealing these enum values from the gh cli's source:
--  pkg/cmd/run/shared/shared.go
-- I'm pretty sure these are shared between jobs and runs, too
data JobStatus
  = Queued
  | Completed
  | InProgress
  | Requested
  | Waiting
  deriving stock (Generic, Show, Eq)

instance JS.FromJSON JobStatus where
  parseJSON = JS.genericParseJSON $ JS.defaultOptions {JS.constructorTagModifier = snakeCase}

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

instance JS.FromJSON JobConclusion where
  parseJSON = JS.genericParseJSON $ JS.defaultOptions {JS.constructorTagModifier = snakeCase}

data FailedJob = FailedJob 
  { fjId :: Int
  , fjName :: Text
  , fjConclusion :: JobConclusion
  , fjStartedAt :: UTCTime
  , fjCompletedAt :: UTCTime
  , fjSteps :: [JobStep]
  }
  deriving stock (Generic, Show, Eq)

failedJobFrom :: JobsProj -> Maybe FailedJob
failedJobFrom JobsProj {..} =
  case jpConclusion of
    Success -> Nothing
    conc -> Just FailedJob 
      { fjId = jpId
      , fjName = jpName
      , fjConclusion = conc
      , fjStartedAt = jpStartedAt
      , fjCompletedAt = jpCompletedAt
      , fjSteps = jpSteps
      }

-- it looks as though /jobs/{jobid}/logs returns the logs for the job as a
-- string, as well as providing a link in the header
-- somehow this doesn't work with httpJSON, but does with httpBS
buildFailedJobLogsRequest :: MonadThrow m => FailedJob -> AppT m Request
buildFailedJobLogsRequest FailedJob {fjId} = do
  let logsReqUri = githubJobLogs fjId
  buildGithubRequest logsReqUri []

githubJobLogs :: Int -> GitRemote -> Text
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
