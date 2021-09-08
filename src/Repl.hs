{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Repl where

import Control.Monad.Catch
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

data StopCondition
  = Expected Text
  | Surprise Text
  deriving stock (Show)

listRuns :: (MonadThrow m, MonadIO m) => 
  Int -> Text -> AppT m (Either StopCondition [CommitRunProj])
listRuns page br = do
  runResp <- runTypedRequestM $ buildListRunsReq [perPage page, branch br]
  runs <- case getResponseStatusCode runResp of
    200 -> pure . Right . lrWorkflowRuns $ getResponseBody runResp
    401 -> pure . Left $ Surprise "Unauthorized"
    403 -> pure . Left $ Surprise "Forbidden"
    404 -> pure . Left $ Expected "Not found"
    s | s < 500 -> pure . Left $ Surprise $ "Some other bad request: " <> fromInt s
    s -> pure . Left $ Surprise $ "Their problem: " <> fromInt s

  case runs of
    Left x -> pure $ Left x
    Right rs | null rs -> pure . Left $ Expected "No runs"
    Right rs -> pure $ Right rs

listJobs :: (MonadThrow m, MonadIO m) => 
  CommitRunProj -> AppT m (Either StopCondition [JobsProj])
listJobs run = do
  jobsResp <- runTypedRequestM $ buildRunJobsRequest run []
  jobs <- case getResponseStatusCode jobsResp of
    200 -> pure . Right . ljJobs $ getResponseBody jobsResp
    401 -> pure . Left $ Surprise "Unauthorized"
    403 -> pure . Left $ Surprise "Forbidden"
    404 -> pure . Left $ Expected "Not found"
    s | s < 500 -> pure . Left . Surprise $ "Some other bad request: " <> fromInt s
    s -> pure . Left . Surprise $ "Their problem: " <> fromInt s

  case jobs of 
    Left x -> pure $ Left x
    Right rs | null rs -> pure . Left . Surprise $ "No jobs for run " <> fromInt (crpId run)
    Right rs -> pure $ Right rs

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

  runs <- listRuns 1 br <&> \case 
    Left x -> error $ show x
    Right rs -> rs

  jobs <- listJobs (head runs) <&> \case
    Left x -> error $ show x
    Right js -> js

  pure $ filter (\j -> (jpStatus j == Completed) && (jpConclusion j /= Success)) jobs 