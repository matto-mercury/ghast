{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Repl where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Aeson as J
import Data.Attoparsec.Text
import Data.Foldable (fold)
import Data.Functor
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text(..), pack, unpack, isSuffixOf)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics
import Network.HTTP.Simple

import Args
import AppEnvironment
import Github
import Github.Jobs as GJ
import Github.Runs as GR
import Parsers.DateTime
import Parsers.Filepath
import Parsers.GhcErrorHeader
import Request
import Shared
import UriFragment
import Util

destructureResponse :: Monad m => (a -> b) -> Response a -> AppT m b
destructureResponse accessor resp = do
  case getResponseStatusCode resp of
    200 -> pure $ accessor $ getResponseBody resp
    401 -> throwError $ Surprise "Unauthorized"
    403 -> throwError $ Surprise "Forbidden"
    404 -> throwError $ Expected "Not found"
    s | s < 500 -> throwError $ Surprise $ "Some other bad request: " <> tshow s
    s -> throwError $ Surprise $ "Their problem: " <> tshow s

listRuns :: (MonadThrow m, MonadIO m) => 
  Int -> Text -> AppT m (NonEmpty CommitRunProj)
listRuns page br = do
  runResp <- runTypedRequestM $ buildListRunsReq [perPage page, Github.branch br]
  -- using >>= instead of <&> because the [] case is doing an unrelated but
  -- monadic operation (throwError); if the case statement was pure we could
  -- just drop the `pure` in x:xs and fmap
  destructureResponse lrWorkflowRuns runResp >>= \case 
    [] -> throwError $ Expected "No GHA runs for current remote and branch"
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

latestFailedRun :: (MonadThrow m, MonadIO m) => AppT m CompletedRun
latestFailedRun = do
  br <- asks gitBranch
  listRuns 1 br >>= failedLatest

specificRun :: (MonadThrow m, MonadIO m) =>
  Int -> AppT m CompletedRun
specificRun runId = do
  runResp <- runTypedRequestM $ buildSpecificRunReq runId
  destructureResponse completedRunFrom runResp >>= \case
    Left s -> throwError . Expected $ "Completed run with ID " <> tshow runId <> " is " <> tshow s
    Right r -> pure r

getInterestingRun :: (MonadThrow m, MonadIO m) => AppT m CompletedRun
getInterestingRun = do
  specific <- asks thisRun
  maybe latestFailedRun specificRun specific

listJobs :: (MonadThrow m, MonadIO m) => 
  CompletedRun -> AppT m (NonEmpty JobsProj)
listJobs run = do
  jobsResp <- runTypedRequestM $ buildRunJobsRequest run []
  destructureResponse ljJobs jobsResp >>= \case
    [] -> throwError $ Surprise $ "No jobs for run " <> tshow (runId run)
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

rawLogsFor :: (MonadThrow m, MonadIO m) =>
  FailedJob -> AppT m Text
rawLogsFor job = do
  logsReq <- buildFailedJobLogsRequest job
  logsResp <- httpBS logsReq
  destructureResponse decodeUtf8 logsResp

doWorkSon :: (MonadThrow m, MonadIO m) => AppT m ()
doWorkSon = do
  remote <- asks gitRemote
  br <- asks gitBranch

  -- this lies if we have a --thisjob param
  liftIO . putStrLn . unpack $ mconcat
    [ "remote: "
    , owner remote
    , "/"
    , repo remote
    , "\nbranch: "
    , br
    ]

  -- jobs <- listRuns 1 br
  --     >>= failedLatest
  jobs <- getInterestingRun
      >>= listJobs
      >>= failedJobs

  liftIO . putStrLn $ fold $ failedJobMessage <$> jobs

  rawLogsText <- rawLogsFor (NEL.head jobs)

  showRawLogs <- asks rawLogs
  let logLines = T.lines rawLogsText

  if showRawLogs then
    liftIO . putStrLn $ unpack rawLogsText
  else
    liftIO . putStrLn $ show (length logLines) ++ " lines of logs"

failedJobMessage :: FailedJob -> String
failedJobMessage FailedJob {..} =
  let jobStepStatus :: JobStep -> String
      jobStepStatus j = unpack (jsName j) ++ ": " ++ show (jsConclusion j)
      jobStepStatuses = intercalate "\n  " $ jobStepStatus <$> fjSteps
   in unpack fjName ++ ": " ++ show fjConclusion ++ "\n  " ++ jobStepStatuses

-- for testing parsers

sampleError :: Text
sampleError = T.intercalate "\n"
  [ "2021-09-17T00:27:22.8377115Z src/Mailer/Disputes/SendCustomerAcknowledgement.hs:14:15: error:"
  , "2021-09-17T00:27:22.8409939Z ##[error]    Not in scope: type constructor or class ‘UserId’"
  , "2021-09-17T00:27:22.8422210Z    |"
  , "2021-09-17T00:27:22.8422816Z 14 |   { userId :: UserId"
  , "2021-09-17T00:27:22.8423238Z    |               ^^^^^^"
  , ""
  ]

sampleErrText :: Text
sampleErrText = T.intercalate "\n"
  [ "2021-09-17T00:27:22.8455815Z ##[error]    • Variable not in scope:"
  , "2021-09-17T00:27:22.8457296Z         shamletFile :: t0 -> Language.Haskell.TH.Lib.Internal.ExpQ"
  , "2021-09-17T00:27:22.8458081Z     • Perhaps you meant ‘whamletFile’ (imported from Import)\n"
  ]