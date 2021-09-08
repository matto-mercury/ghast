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
import Github.Jobs
import Github.Runs
import Request
import Shared
import UriFragment

fromInt :: Int -> Text
fromInt = pack . show

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

  jobs <- listRuns 1 br
     >>= (listJobs . head)

  pure $ filter (\j -> (jpStatus j == Completed) && (jpConclusion j /= Success)) jobs 