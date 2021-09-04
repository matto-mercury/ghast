module AppEnvironment where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Attoparsec.Text
import Data.Text (Text(..), pack, unpack, isSuffixOf)
import qualified Data.Text as T (lines) 
import Shelly
import System.Environment

newtype AppT m a = AppT { runAppT :: ReaderT Env m a }
  deriving newtype (MonadReader Env, Monad, Applicative, Functor, MonadIO, MonadThrow)

data Env = Env
  { userId :: Text
  , passwd :: Text
  , gitBranch :: Text
  , gitRemote :: GitRemote
  }
  deriving Show

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