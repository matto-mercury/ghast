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
import Parsers.GhcErrors
import Parsers.GithubLogs
import Parsers.MigrationMismatch
import Parsers.OtherLogline
import Request
import Shared
import UriFragment

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
  banner <- repoConfigMessage
  liftIO $ putStrLn banner

  run <- getInterestingRun
  jobs <- failedJobs =<< listJobs run

  liftIO . putStrLn $ intercalate "\n\n" $ toList $ failedJobMessage <$> jobs
  liftIO $ putStrLn "\nDetails:\n"

  rawLogsText <- rawLogsFor (NEL.head jobs)

  showRawLogs <- asks rawLogs

  if showRawLogs then
    liftIO . putStrLn $ unpack rawLogsText
  else
    liftIO . putStrLn . prettify $ parseGithubJobLogs rawLogsText

  let runIdStr = show $ runId run -- why I can't inline this is beyond me
  liftIO . putStrLn $ "\nRunID: " ++ runIdStr

repoConfigMessage :: (MonadThrow m, MonadIO m) => AppT m String 
repoConfigMessage = do
  remote <- asks gitRemote

  let remoteStr = unpack $ mconcat
        [ "remote: "
        , owner remote
        , "/"
        , repo remote
        , "\n"
        ]

  targetStr <- runTargetString

  pure $ remoteStr ++ targetStr

runTargetString :: (MonadThrow m, MonadIO m) => AppT m String
runTargetString = do
  branch <- asks gitBranch
  run <- asks thisRun

  pure $ case (branch, run) of
    (br, Nothing) -> "branch: " ++ unpack br
    (_, Just run) -> "run ID: " ++ show run

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

sampleWarning :: Text
sampleWarning = T.intercalate "\n"
  [ "2021-09-17T00:27:22.8473532Z src/Consumer/Jobs/SendDisputeEmails.hs:5:1: error: [-Wunused-imports, -Werror=unused-imports]"
  , "2021-09-17T00:27:22.8474713Z ##[error]    The import of ‘Mercury.Banking.Types.Core’ is redundant"
  , "2021-09-17T00:27:22.8476135Z       except perhaps to import instances from ‘Mercury.Banking.Types.Core’"
  , "2021-09-17T00:27:22.8476861Z     To import instances alone, use: import Mercury.Banking.Types.Core()"
  , "2021-09-17T00:27:22.8477313Z   |"
  , "2021-09-17T00:27:22.8477837Z 5 | import Mercury.Banking.Types.Core (transactionAmountToDollar)"
  , "2021-09-17T00:27:22.8478476Z   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n"
  ]

sampleErrText :: Text
sampleErrText = T.intercalate "\n"
  [ "2021-09-17T00:27:22.8455815Z ##[error]    • Variable not in scope:"
  , "2021-09-17T00:27:22.8457296Z         shamletFile :: t0 -> Language.Haskell.TH.Lib.Internal.ExpQ"
  , "2021-09-17T00:27:22.8458081Z     • Perhaps you meant ‘whamletFile’ (imported from Import)\n"
  ]

sampleGarbageText :: Text
sampleGarbageText = T.intercalate "\n"
  [ "2021-09-17T00:27:19.7084858Z [1407 of 1441] Compiling Tasks.SendFunderaSignupsReport ( src/Tasks/SendFunderaSignupsReport.hs, dist/build/Tasks/SendFunderaSignupsReport.o, dist/build/Tasks/SendFunderaSignupsReport.dyn_o )"
  , "2021-09-17T00:27:19.7086922Z [1408 of 1441] Compiling Tasks.SendOutgoingChoiceAchFile ( src/Tasks/SendOutgoingChoiceAchFile.hs, dist/build/Tasks/SendOutgoingChoiceAchFile.o, dist/build/Tasks/SendOutgoingChoiceAchFile.dyn_o )"
  , "2021-09-17T00:27:19.7088677Z [1409 of 1441] Compiling Tasks.SendPushNotification ( src/Tasks/SendPushNotification.hs, dist/build/Tasks/SendPushNotification.o, dist/build/Tasks/SendPushNotification.dyn_o )"
  , "2021-09-17T00:27:19.7090448Z [1410 of 1441] Compiling Tasks.StuckSynapseCheckDeposits ( src/Tasks/StuckSynapseCheckDeposits.hs, dist/build/Tasks/StuckSynapseCheckDeposits.o, dist/build/Tasks/StuckSynapseCheckDeposits.dyn_o )\n"
  ]

sampleSmallMigrationMismatchText :: Text
sampleSmallMigrationMismatchText = T.intercalate "\n"
  [ "2021-09-29T21:46:07.0506463Z If you have an intentional mismatch, you can add it to the `intentionalMismatches` list in Application.hs."
  , "2021-09-29T21:46:07.0507688Z ALTER TABLE \"urbanft_transactions\" ALTER COLUMN \"check_number\" TYPE non_empty_text_299"
  , "2021-09-29T21:46:07.0507692Z ALTER TABLE \"something_else\" ALTER COLUMN \"check_number\" TYPE non_empty_text_299"
  , "2021-09-29T21:46:07.0508629Z test: MigrationMismatch \"Please fix your migrations\"\n"
  ]

sampleMigrationMismatchText :: Text
sampleMigrationMismatchText = T.intercalate "\n"
  [ "2021-09-29T21:46:01.4845114Z WARNING:  there is no transaction in progress"
  , "2021-09-29T21:46:03.0747026Z Running 1 test suites..."
  , "2021-09-29T21:46:03.0758403Z Test suite test: RUNNING..."
  , "2021-09-29T21:46:07.0503525Z Warning: The following migrations may be necessary to match the DB schema to Persistent's models."
  , "2021-09-29T21:46:07.0505061Z (If the migration is spurious, see if you can fix Persistent or hardcode something to ignore the migration)."
  , "2021-09-29T21:46:07.0506463Z If you have an intentional mismatch, you can add it to the `intentionalMismatches` list in Application.hs."
  , "2021-09-29T21:46:07.0507688Z ALTER TABLE \"urbanft_transactions\" ALTER COLUMN \"check_number\" TYPE non_empty_text_299"
  , "2021-09-29T21:46:07.0507692Z ALTER TABLE \"something_else\" ALTER COLUMN \"check_number\" TYPE non_empty_text_299"
  , "2021-09-29T21:46:07.0508629Z test: MigrationMismatch \"Please fix your migrations\""
  , "2021-09-29T21:46:07.0509313Z Test suite test: FAIL"
  , "2021-09-29T21:46:07.0510265Z Test suite logged to: dist/test/mwb-0-test.log"
  , "2021-09-29T21:46:07.0510930Z 0 of 1 test suites (0 of 1 test cases) passed.\n"
  ]

sampleCorpus :: Text
sampleCorpus = mconcat
  [ sampleGarbageText
  , sampleError
  , sampleWarning
  , sampleGarbageText
  , sampleError
  , sampleGarbageText
  , sampleWarning
  , sampleGarbageText
  ]
