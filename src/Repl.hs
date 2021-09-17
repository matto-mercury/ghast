{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Repl where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Aeson as J
import Data.Functor
import Data.Text (Text(..), pack, unpack, isSuffixOf)
import GHC.Generics
import Network.HTTP.Simple

import AppEnvironment
import Github
import Github.Jobs as GJ
import Github.Runs as GR
import Request
import Shared
import UriFragment

fromInt :: Int -> Text
fromInt = pack . show

tshow :: Show a => a -> Text
tshow = pack . show

-- could be CPS instead of Either, but this is good for now
listRuns :: (MonadThrow m, MonadIO m) => 
  Int -> Text -> AppT m [CommitRunProj]
listRuns page br = do
  runResp <- runTypedRequestM $ buildListRunsReq [perPage page, branch br]
  runs <- case getResponseStatusCode runResp of
    200 -> pure $ lrWorkflowRuns $ getResponseBody runResp
    401 -> throwError $ Surprise "Unauthorized"
    403 -> throwError $ Surprise "Forbidden"
    404 -> throwError $ Expected "Not found"
    s | s < 500 -> throwError $ Surprise $ "Some other bad request: " <> fromInt s
    s -> throwError $ Surprise $ "Their problem: " <> fromInt s

  if null runs then throwError $ Expected "No runs"
  else pure runs

failedLatest :: (MonadThrow m, MonadIO m) =>
  [CommitRunProj] -> AppT m CommitRunProj
failedLatest runs = do
  let latest = head runs
  status <- case crpStatus latest of
    GR.Completed -> pure GR.Completed
    s -> throwError $ Expected $ "Job status: " <> tshow s
  conclusion <- case crpConclusion latest of
    Just GR.Success -> throwError $ Expected "Job succeeded"  
    Just c -> pure c
    Nothing -> throwError $ Surprise "ope; job isn't complete but checked conclusion for some reason"

  pure latest

listJobs :: (MonadThrow m, MonadIO m) => 
  CommitRunProj -> AppT m [JobsProj]
listJobs run = do
  jobsResp <- runTypedRequestM $ buildRunJobsRequest run []
  jobs <- case getResponseStatusCode jobsResp of
    200 -> pure $ ljJobs $ getResponseBody jobsResp
    401 -> throwError $ Surprise "Unauthorized"
    403 -> throwError $ Surprise "Forbidden"
    404 -> throwError $ Expected "Not found"
    s | s < 500 -> throwError . Surprise $ "Some other bad request: " <> fromInt s
    s -> throwError . Surprise $ "Their problem: " <> fromInt s

  if null jobs then throwError . Surprise $ "No jobs for run " <> fromInt (crpId run)
  else pure jobs

failedJobs :: (MonadThrow m, MonadIO m) =>
  [JobsProj] -> AppT m [JobsProj]
failedJobs jobs = do
  let failed = filter (\j -> (jpStatus j == GJ.Completed) && (jpConclusion j /= GJ.Success)) jobs 
  case failed of
    [] -> throwError $ Expected "No failed jobs"
    xs -> pure xs

-- jeez
doWorkSon :: (MonadThrow m, MonadIO m) => AppT m ()
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

  jobs <- listRuns 1 br
      >>= failedLatest
      >>= listJobs
      >>= failedJobs

  liftIO $ print jobs