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

data CommitRunProj = CommitRunProj
  { crpId :: Int
  , crpHeadBranch :: Text
  , crpStatus :: Text
  , crpConclusion :: Text
  , crpJobsUrl :: Text
  , crpLogsUrl :: Text -- sends back a zipfile, apparently, probably not what I want
  }
  deriving stock (Generic, Show, Eq)

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