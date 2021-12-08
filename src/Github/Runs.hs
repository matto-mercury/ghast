module Github.Runs where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Text (unpack)
import Data.Time.Clock
import GHC.Generics
import Network.HTTP.Simple

import LocalPrelude

import AppEnvironment
import Github
import Github.Jobs (ListJobs (..))
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
  , crpCreatedAt :: UTCTime
  , crpUpdatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON CommitRunProj where
  parseJSON = genericParseJSON $ aesonOptions $ Just "crp"

githubGetRun :: Int -> GitRemote -> Text
githubGetRun runId GitRemote {..} = 
  mconcat 
    [ "/repos/"
    , owner
    , "/"
    , repo
    , "/actions/runs/"
    , tshow runId
    ]

buildSpecificRunReq :: MonadThrow m => Int -> AppT m (TypedRequest CommitRunProj)
buildSpecificRunReq runId =
  TypedRequest <$> buildGithubRequest (githubGetRun runId) []

data CompletedRun = CompletedRun
  { runId :: Int
  , conclusion :: RunConclusion
  , jobsUrl :: Text
  }
  deriving stock (Generic, Show, Eq)

completedRunFrom :: CommitRunProj -> Either RunStatus CompletedRun
completedRunFrom CommitRunProj {..} =
  case (crpStatus, crpConclusion) of
    -- this is a bit janky... I'm fairly confident that if status is Completed
    --  there will be a conclusion, but it's not clear to me how to encode that
    --  in a less sketchy way. we're throwing away any (incomplete, conclusion)
    --  results, which are probably interesting from a perverse edge case POV,
    --  but not super relevant to the task at hand
    (Completed, Just c) -> Right CompletedRun 
      { runId = crpId, conclusion = c, jobsUrl = crpJobsUrl }
    (_, _) -> Left crpStatus

buildRunJobsRequest :: MonadThrow m => CompletedRun -> [Parameter] -> AppT m (TypedRequest ListJobs)
buildRunJobsRequest CompletedRun {jobsUrl} params = do -- NamedFieldPuns instead of {..}
  (u, p) <- asks getCreds
  uaReq <- setUserAgent <$> parseRequest (unpack $ jobsUrl <> render params)
  pure $ TypedRequest $ authenticateWithBasic u p uaReq