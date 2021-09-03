{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Repl where

import Control.Monad.Catch
import Control.Monad.Reader
import qualified Data.Aeson as J
import Data.Attoparsec.Text
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Base64 as B64
import Data.Aeson.Casing (snakeCase)
import Data.Text (Text(..), pack, unpack, isSuffixOf)
import qualified Data.Text as T (lines) 
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Network.HTTP.Simple
import Shelly
import System.Environment

import UriFragment

getUserData :: MonadThrow m => m Request
getUserData = parseRequest "https://api.github.com/users/matto-mercury"

setUserAgent :: Request -> Request
setUserAgent bareReq =
  let (name, val) = ("User-Agent", "ghast-0.1.0.0") in
  addRequestHeader name val bareReq

authenticateWithBasic :: Text -> Text -> Request -> Request
authenticateWithBasic user pass =
  let u = encodeUtf8 user
      p = encodeUtf8 pass
  in
  addRequestHeader "Authorization" $ B8.pack "Basic " <> B64.encode (u <> ":" <> p)

newtype TypedRequest response =
  TypedRequest { unTypedRequest :: Request }

runTypedRequest :: MonadIO io => J.FromJSON a => TypedRequest a -> io (Response a)
runTypedRequest req = do
  httpJSON (unTypedRequest req)

runTypedRequestM :: MonadIO m => J.FromJSON a => AppT m (TypedRequest a) -> AppT m (Response a)
runTypedRequestM req = runTypedRequest =<< req

buildGithubRequest :: MonadThrow m => (GitRemote -> Text) -> [Parameter] -> AppT m Request
buildGithubRequest pathFunc params = do
  (u, p) <- asks getCreds
  remote <- asks gitRemote
  let path = pathFunc remote
  uaReq <- setUserAgent <$> parseRequest (unpack $ "https://api.github.com" <> path <> render params)
  pure $ authenticateWithBasic u p uaReq

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
  TypedRequest @ListRuns <$> buildGithubRequest githubListRuns params

perPage :: Integer -> Parameter
perPage n = Param "per_page" (pack $ show n)

branch :: Text -> Parameter
branch = Param "branch"

aesonOptions :: Maybe String -> J.Options
aesonOptions ms = J.defaultOptions { J.fieldLabelModifier = snakeCase . drop len }
  where len = maybe 0 length ms

data UserDataProj = UserDataProj 
  { udpId :: Integer
  , udpName :: String
  , udpNodeId :: String
  }
  deriving stock (Generic, Show, Eq)

instance J.FromJSON UserDataProj where
  parseJSON = J.genericParseJSON $ aesonOptions $ Just "udp"
  -- parseJSON = withObject "UserDataProj" $ \v -> UserDataProj
  --   <$> v .: "id"
  --   <*> v .: "name"
  --   <*> v .: "node_id"

data ListRuns = ListRuns
  { lrTotalCount :: Integer
  , lrWorkflowRuns :: [CommitRunProj]
  }
  deriving stock (Generic, Show, Eq)

instance J.FromJSON ListRuns where
  parseJSON = J.genericParseJSON $ aesonOptions $ Just "lr"

data CommitRunProj = CommitRunProj
  { crpId :: Integer
  , crpHeadBranch :: Text
  , crpStatus :: Text
  , crpConclusion :: Text
  , crpJobsUrl :: Text
  , crpLogsUrl :: Text -- sends back a zipfile, apparently, probably not what I want
  }
  deriving stock (Generic, Show, Eq)

instance J.FromJSON CommitRunProj where
  parseJSON = J.genericParseJSON $ aesonOptions $ Just "crp"

buildRunJobsRequest :: MonadThrow m => CommitRunProj -> [Parameter] -> AppT m (TypedRequest ListJobs)
buildRunJobsRequest CommitRunProj {crpJobsUrl} params = do -- NamedFieldPuns instead of {..}
  (u, p) <- asks getCreds
  uaReq <- setUserAgent <$> parseRequest (unpack $ crpJobsUrl <> render params)
  pure $ TypedRequest @ListJobs $ authenticateWithBasic u p uaReq



-- buildRunLogsRequest :: MonadThrow m => CommitRunProj -> [Parameter] -> AppT m Request
-- runLogsRequest CommitRunProj {crpLogsUrl} params = do
--   (u, p) <- asks getCreds
--   uaReq <- setUserAgent <$> parseRequest (unpack $ crpLogsUrl <> render params)
--   pure $ authenticateWithBasic u p uaReq

data ListJobs = ListJobs
  { ljTotalCount :: Integer
  , ljJobs :: [JobsProj]
  }
  deriving stock (Generic, Show, Eq)

instance J.FromJSON ListJobs where
  parseJSON = J.genericParseJSON $ aesonOptions $ Just "lj"

data JobsProj = JobsProj
  { jpId :: Integer
  , jpRunId :: Integer
  , jpStatus :: JobStatus
  , jpConclusion :: JobConclusion
  , jpStartedAt :: String
  , jpCompletedAt :: String
  , jpName :: String
  , jpSteps :: [JobSteps]
  }
  deriving stock (Generic, Show, Eq)

instance J.FromJSON JobsProj where
  parseJSON = J.genericParseJSON $ aesonOptions $ Just "jp"

data JobSteps = JobSteps
  { jsName :: String
  , jsStatus :: JobStatus
  , jsConclusion :: JobConclusion
  , jsNumber :: Integer
  , jsStartedAt :: String
  , jsCompletedAt :: String
  }
  deriving stock (Generic, Show, Eq)

instance J.FromJSON JobSteps where
  parseJSON = J.genericParseJSON $ aesonOptions $ Just "js"

-- these aren't really documented anywhere that I can find in the API guide,
-- but I'm stealing these enum values from the gh cli's source:
--  pkg/cmd/run/shared/shared.go
data JobStatus
  = Queued
  | Completed
  | InProgress
  | Requested
  | Waiting
  deriving stock (Generic, Show, Eq)

instance J.FromJSON JobStatus where
  parseJSON = J.genericParseJSON $ J.defaultOptions {J.constructorTagModifier = snakeCase}

data JobConclusion
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

instance J.FromJSON JobConclusion where
  parseJSON = J.genericParseJSON $ J.defaultOptions {J.constructorTagModifier = snakeCase}

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

  runReq <- buildGithubRequest githubListRuns [perPage 1, branch br]
  runResp <- httpJSON @_ @ListRuns runReq

  let runs = lrWorkflowRuns $ getResponseBody runResp

  -- jobsReq <- buildRunJobsRequest (head runs) []
  -- jobsResp <- runTypedRequest jobsReq
  jobsResp <- runTypedRequestM $ buildRunJobsRequest (head runs) []

  let jobs = ljJobs $ getResponseBody jobsResp

  pure $ filter (\j -> (jpStatus j == Completed) && (jpConclusion j /= Success)) jobs 

data Env = Env
  { userId :: Text
  , passwd :: Text
  , gitBranch :: Text
  , gitRemote :: GitRemote
  }
  deriving Show

getCreds :: Env -> (Text, Text)
getCreds env = (userId env, passwd env)

newtype AppT m a = AppT { runAppT :: ReaderT Env m a }
  deriving newtype (MonadReader Env, Monad, Applicative, Functor, MonadIO, MonadThrow)

getEnvironmentUserid :: IO (Maybe String)
getEnvironmentUserid = lookupEnv "GITHUB_USER"

getEnvironmentPassword :: IO (Maybe String)
getEnvironmentPassword = lookupEnv "GITHUB_KEY"

data GitRemote = GitRemote
  { owner :: Text
  , repo :: Text
  }
  deriving Show

-- given the output of `git remote -v`, finds the first line with "(push)" at
-- the end. first version is unsafe on "none of them"
findPushRemote :: Text -> Text
findPushRemote remotesText =
  let remotes = T.lines remotesText
      pushes = filter (isSuffixOf "(push)") remotes
   in
      head pushes

parsePushRemote :: Parser GitRemote
parsePushRemote = do
  string "origin\tgit@github.com:"
  owner <- takeTill (== '/')
  char '/'
  repo <- takeTill (== '.')
  pure GitRemote { owner = owner, repo = repo }

parseBranch :: Parser Text
parseBranch = "refs/heads/" *> takeText

readEnvCreds :: IO Env
readEnvCreds = do
  mUserid <- getEnvironmentUserid
  mPasswd <- getEnvironmentPassword

  (branch, remote) <- shelly $ silently $ do
    b <- run "git" ["symbolic-ref", "--quiet", "HEAD"]
    r <- findPushRemote <$> run "git" ["remote", "-v"]
    pure (b, r)

  let eBranch = parseOnly parseBranch branch
  let eRemote = parseOnly parsePushRemote remote

  case (mUserid, mPasswd, eBranch, eRemote) of
    (Just u, Just p, Right b, Right r) -> pure Env 
      { userId = pack u
      , passwd = pack p
      , gitBranch = b
      , gitRemote = r
      }
    (_, _, _, _) -> error "oops"

runAppEnv :: AppT IO a -> IO a
runAppEnv app = do
  env <- readEnvCreds
  runReaderT (runAppT app) env
  -- case app of
  --   AppT a -> case a of
  --     ReaderT ema -> ema env

-- repl test framework
-- depends on real creds in the env but brings in hardcoded git stuff

readTestCreds :: IO Env
readTestCreds = do
  mUserid <- getEnvironmentUserid
  mPasswd <- getEnvironmentPassword

  case (mUserid, mPasswd) of
    (Just u, Just p) -> pure Env
      { userId = pack u
      , passwd = pack p
      , gitBranch = "matto/rul-88"
      , gitRemote = testRemote
      }

testRemote :: GitRemote
testRemote = GitRemote
  { owner = "MercuryTechnologies"
  , repo  = "mercury-web-backend"
  }

runFwk :: AppT IO a -> IO a
runFwk app = do
  env <- readTestCreds
  runReaderT (runAppT app) env