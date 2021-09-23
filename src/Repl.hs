{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Repl where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Aeson as J
import Data.Foldable (fold)
import Data.Functor
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.List.NonEmpty as NEL
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

destructureResponse :: Monad m => (a -> b) -> Response a -> AppT m b
destructureResponse accessor resp = do
  case getResponseStatusCode resp of
    200 -> pure $ accessor $ getResponseBody resp
    401 -> throwError $ Surprise "Unauthorized"
    403 -> throwError $ Surprise "Forbidden"
    404 -> throwError $ Expected "Not found"
    s | s < 500 -> throwError $ Surprise $ "Some other bad request: " <> fromInt s
    s -> throwError $ Surprise $ "Their problem: " <> fromInt s

listRuns :: (MonadThrow m, MonadIO m) => 
  Int -> Text -> AppT m (NonEmpty CommitRunProj)
listRuns page br = do
  runResp <- runTypedRequestM $ buildListRunsReq [perPage page, branch br]
  -- using >>= instead of <&> because the [] case is doing an unrelated but
  -- monadic operation (throwError); if the case statement was pure we could
  -- just drop the `pure` in x:xs and fmap
  destructureResponse lrWorkflowRuns runResp >>= \case 
    [] -> throwError $ Expected "No runs"
    x:xs -> pure $ x :| xs

failedLatest :: (MonadThrow m, MonadIO m) =>
  NonEmpty CommitRunProj -> AppT m CompletedRun
failedLatest runs = do
  let latest = NEL.head runs
  completedLatest <- case completedRunFrom latest of
    Right run -> pure run 
    Left status -> throwError $ Expected $ "Job status: " <> tshow status
  if conclusion completedLatest == GR.Success
  then throwError $ Expected "Job succeeded"
  else pure completedLatest

listJobs :: (MonadThrow m, MonadIO m) => 
  CompletedRun -> AppT m (NonEmpty JobsProj)
listJobs run = do
  jobsResp <- runTypedRequestM $ buildRunJobsRequest run []
  destructureResponse ljJobs jobsResp >>= \case
    [] -> throwError $ Surprise $ "No jobs for run " <> fromInt (runId run)
    x:xs -> pure $ x :| xs

failedJobs :: (MonadThrow m, MonadIO m) =>
  NonEmpty JobsProj -> AppT m (NonEmpty FailedJob)
failedJobs jobs = do
  let failed = NEL.filter (\j -> (jpStatus j == GJ.Completed) && (jpConclusion j /= GJ.Success)) jobs 
  let convert :: (MonadThrow m) => JobsProj -> AppT m FailedJob
      convert fj = case failedJobFrom fj of
        Nothing -> throwError $ Surprise $ "Job " <> tshow fj <> " slipped through the failed filter"
        Just j -> pure j
  case failed of
    [] -> throwError $ Expected "No failed jobs"
    x:xs -> sequenceA $ convert <$> (x :| xs)

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

  liftIO . putStrLn $ fold $ failedJobMessage <$> jobs

failedJobMessage :: FailedJob -> String
failedJobMessage FailedJob {..} =
  let jobStepStatus :: JobStep -> String
      jobStepStatus j = unpack (jsName j) ++ ": " ++ show (jsConclusion j)
      jobStepStatuses = intercalate "\n  " $ jobStepStatus <$> fjSteps
   in unpack fjName ++ ": " ++ show fjConclusion ++ "\n  " ++ jobStepStatuses