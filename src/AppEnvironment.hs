module AppEnvironment where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Attoparsec.Text
import Data.Text (Text(..), pack, unpack, isSuffixOf)
import qualified Data.Text as T (lines) 
import Options.Generic
import Shelly
import System.Environment

import Args
import Parsers.GithubLogs (BuildError, renderDefaultBuildErrors)

data StopCondition
  = Expected Text
  | Surprise Text
  deriving stock (Show)

newtype AppT m a = AppT { runAppT :: ReaderT Env (ExceptT StopCondition m) a }
  deriving newtype
   ( MonadReader Env
   , MonadError StopCondition
   , Monad, Applicative, Functor, MonadIO, MonadThrow
   )

data Env = Env
  { userId :: Text -- always from env
  , passwd :: Text -- always from env
  , gitBranch :: Text -- from env or cmd line
  , gitRemote :: GitRemote -- always from env
  , rawLogs :: Bool -- always from cmd line
  , thisRun :: Maybe Int
  , renderer :: [BuildError] -> String
  }

getCreds :: Env -> (Text, Text)
getCreds env = (userId env, passwd env)

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

  let creds = (,) <$> mUserid <*> mPasswd
  let gitEnv = (,) <$> eBranch <*> eRemote

  case (creds, gitEnv) of
    (Just (u, p), Right (b, r)) -> pure Env 
      { userId = pack u
      , passwd = pack p
      , gitBranch = b
      , gitRemote = r
      , rawLogs = False
      , thisRun = Nothing
      , renderer = renderDefaultBuildErrors
      }
    (Nothing, _) -> error "Github creds missing"
    (_, Left err) -> error $ "Failed to parse environment: " ++ err

fillArgs :: Args -> Env -> Env
fillArgs Args {..} env =
  env 
    { rawLogs = rawlogs
    , thisRun = thisrun
    , gitBranch =
      case branch of
        Nothing -> gitBranch env
        Just b -> b
    , renderer =
      case verbosity of
        Nothing -> renderer env
        Just v -> pickRenderer v
    }

pickRenderer :: RenderVerbosity -> [BuildError] -> String
pickRenderer _ = renderDefaultBuildErrors

-- this is IO () so showStop can typecheck with putStrLn, which is IO ()
-- it's a bit of a hack but will do for now
runAppEnv :: AppT IO () -> IO ()
runAppEnv app = do
  args <- getRecord "ghast"
  env <- readEnvCreds
  eResult <- runExceptT $ runReaderT (runAppT app) $ fillArgs args env
  case eResult of
    Left x -> showStop x
    Right v -> pure v
  -- case app of
  --   AppT a -> case a of
  --     ReaderT ema -> ema env

showStop :: StopCondition -> IO () 
showStop stop = do
  case stop of
    Expected e -> putStrLn $ unpack e
    Surprise s -> error $ unpack s

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
      , rawLogs = False
      , thisRun = Nothing
      , renderer = renderDefaultBuildErrors
      }

testRemote :: GitRemote
testRemote = GitRemote
  { owner = "MercuryTechnologies"
  , repo  = "mercury-web-backend"
  }

runFwk :: AppT IO a -> IO a 
runFwk app = do
  env <- readTestCreds
  eResult <- runExceptT $ runReaderT (runAppT app) env
  case eResult of
    Left x -> error $ show x
    Right v -> pure v
