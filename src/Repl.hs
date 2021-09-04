{-# LANGUAGE FlexibleInstances #-}

module Repl where

import Control.Monad.Catch
import Control.Monad.Reader
import qualified Data.Aeson as J
import Data.Text (Text(..), pack, unpack, isSuffixOf)
import GHC.Generics
import Network.HTTP.Simple

import AppEnvironment
import Github
import Github.Jobs
import Github.Runs
import Request
import Shared
import UriFragment

-- jeez
doWorkSon :: (MonadThrow m, MonadIO m) => AppT m [JobsProj]
doWorkSon = do
  remote <- asks gitRemote
  br <- asks gitBranch

  liftIO . putStrLn . unpack $ mconcat
    [ "remote: "
    , owner remote
    , "/"
    , repo remote
    , "\nbranch: "
    , br
    ]

  runResp <- runTypedRequestM $ buildListRunsReq [perPage 1, branch br]
  let runs = lrWorkflowRuns $ getResponseBody runResp

  jobsResp <- runTypedRequestM $ buildRunJobsRequest (head runs) []
  let jobs = ljJobs $ getResponseBody jobsResp

  pure $ filter (\j -> (jpStatus j == Completed) && (jpConclusion j /= Success)) jobs 