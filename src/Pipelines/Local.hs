{-
Design:

config:
workdir
plan json paths

validation:
workdir exists
plan json paths exist and parse correctly

preparation:
create plan directories
- create input subdir
- create task subdir
- create state subdir
- create archive subdir

execution:
watch input dir
invoke plans on new inputs
update state dir with state changes
if finished, move input to archive
can cancel by moving input to archive manually

remote access:
run web interface that can
- list running plans
- list archived plans
- create new inputs
- stop running plans
- show plan state
- clone and restart archived plans
-}
module Pipelines.Local where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Aeson as A
import Data.Typeable
import qualified Data.Text as T
import Options.Applicative
import List.Transformer
import Pipelines.Common
import Pipelines.Coordination
import Pipelines.Execution
import Pipelines.Filesystem
import Pipelines.Types

data Options = Options
  { _optionsBaseDir  :: FilePath
  , _optionsPlanFile :: FilePath
  } deriving (Show, Eq)

opts :: Parser Options
opts =
  Options <$>
    strOption (long "base" <> metavar "BASE" <> help "path to listen to") <*>
    strOption (long "plan" <> metavar "PLAN" <> help "plan")

instance MonadCommand IO where
  command path action timeout = putStrLn (T.unpack action) >> return OkResult
  
runLocal :: IO ()
runLocal = execParser parser >>= convert >>= process >>= drain
  where
    parser = info (helper <*> opts)
      ( fullDesc
        <> progDesc "Run each PLAN under the BASE directory"
        <> header "pipelines" )

data FailedToDecode = FailedToDecode FilePath deriving (Show, Eq, Typeable)
instance Exception FailedToDecode

convert :: (MonadFS m, MonadThrow m) => Options -> m CoordinationEnv
convert (Options baseDir planFile) = do
  createDirectoryIfMissingFS False baseDir
  contents <- readFileFS planFile
  case A.decode contents of
    Nothing -> throwM $ FailedToDecode planFile
    Just plan -> return $ CoordinationEnv baseDir plan

process :: (MonadIO m, MonadFS m, MonadWatch m, MonadThrow m, MonadCommand m) =>
           CoordinationEnv -> m (Watch m (Plan, Task, Result))
process (CoordinationEnv dir plans) = do
  Watch list stop <- coordinate dir plans
  let list2 = list >>= \(plan, env) -> execute plan env >>= \(task, result) -> return (plan, task, result)
  return (Watch list2 stop)

drain :: Monad m => Watch m a -> m ()
drain (Watch list stop) = runListT list >> stop
