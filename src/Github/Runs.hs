module Github.Runs where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Text
import GHC.Generics
import Network.HTTP.Simple

import AppEnvironment
import Github
import Github.Jobs
import Request
import Shared
import UriFragment


githubListRuns :: GitRemote -> Text
githubListRuns GitRemote {..} = 
  mconcat 
    [ "/repos/"
    , owner
    , "/"
    , repo
    , "/actions/runs"
    ]

buildListRunsReq :: MonadThrow m => [Parameter] -> AppT m (TypedRequest ListRuns)
buildListRunsReq params = 
  TypedRequest <$> buildGithubRequest githubListRuns params

data ListRuns = ListRuns
  { lrTotalCount :: Int
  , lrWorkflowRuns :: [CommitRunProj]
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON ListRuns where
  parseJSON = genericParseJSON $ aesonOptions $ Just "lr"

-- these aren't really documented anywhere that I can find in the API guide,
-- but I'm stealing these enum values from the gh cli's source:
--  pkg/cmd/run/shared/shared.go
-- I'm pretty sure these are shared between jobs and runs, too
data RunStatus
  = Queued
  | Completed
  | InProgress
  | Requested
  | Waiting
  deriving stock (Generic, Show, Eq)

instance FromJSON RunStatus where
  parseJSON = genericParseJSON $ defaultOptions {constructorTagModifier = snakeCase}

data RunConclusion
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

instance FromJSON RunConclusion where
  parseJSON = genericParseJSON $ defaultOptions {constructorTagModifier = snakeCase}

data CommitRunProj = CommitRunProj
  { crpId :: Int
  , crpHeadBranch :: Text
  , crpStatus :: RunStatus
  , crpConclusion :: Maybe RunConclusion -- got "("conclusion",Null)" back as a parse result
  , crpJobsUrl :: Text
  , crpLogsUrl :: Text -- sends back a zipfile, apparently, probably not what I want
  }
  deriving stock (Generic, Show, Eq)

-- TODO: pull out crpStatus and crpConclusion from github cli golang source

instance FromJSON CommitRunProj where
  parseJSON = genericParseJSON $ aesonOptions $ Just "crp"

buildRunJobsRequest :: MonadThrow m => CommitRunProj -> [Parameter] -> AppT m (TypedRequest ListJobs)
buildRunJobsRequest CommitRunProj {crpJobsUrl} params = do -- NamedFieldPuns instead of {..}
  (u, p) <- asks getCreds
  uaReq <- setUserAgent <$> parseRequest (unpack $ crpJobsUrl <> render params)
  pure $ TypedRequest $ authenticateWithBasic u p uaReq

-- buildRunLogsRequest :: MonadThrow m => CommitRunProj -> [Parameter] -> AppT m Request
-- runLogsRequest CommitRunProj {crpLogsUrl} params = do
--   (u, p) <- asks getCreds
--   uaReq <- setUserAgent <$> parseRequest (unpack $ crpLogsUrl <> render params)
--   pure $ authenticateWithBasic u p uaReq