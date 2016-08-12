module Pipelines.Execution
  ( ExecutionEnv(..)
  , History(..)
  , execute
  ) where

import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson             ((.:), (.=))
import qualified Data.Aeson             as A
import qualified Data.Aeson.Types       as A
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Set               as S
import qualified Data.Text              as T
import           List.Transformer
import           Pipelines.Common
import           Pipelines.Core
import           Pipelines.Filesystem
import           Pipelines.Types
import           System.FilePath

-- | Relevant information about the completed execution of a Task
data History = History
  { _historyTaskName :: Name
  , _historyResult   :: Result
  } deriving (Show, Eq)

instance A.FromJSON History where
  parseJSON (A.Object m) =
    History <$>
      m .: "task_name"   <*>
      m .: "result"
  parseJSON invalid = A.typeMismatch "History" invalid

instance A.ToJSON History where
  toJSON (History taskName result) = A.object
    [ "task_name" .= taskName
    , "result" .= result
    ]

data ExecutionEnv = ExecutionEnv
  { _executionEnvPlanDir :: FilePath
  , _executionEnvInput   :: FilePath
  } deriving (Show, Eq)

data ExecutionState = ExecutionState
  { _executionStateName      :: Name
  , _executionStateHistories :: [History]
  } deriving (Show, Eq)

instance A.FromJSON ExecutionState where
  parseJSON (A.Object m) =
    ExecutionState <$>
      m .: "name" <*>
      m .: "histories"
  parseJSON invalid = A.typeMismatch "ExecutionState" invalid

instance A.ToJSON ExecutionState where
  toJSON (ExecutionState name histories) = A.object
    [ "name" .= name
    , "histories" .= histories
    ]

type MonadExecution b m = (Monad b, MonadFS b, MonadCommand b, Monad m, MonadBase b m,
                           MonadReader ExecutionEnv m, MonadState ExecutionState m)

askStateFile :: MonadExecution b m => m FilePath
askStateFile = do
  execDir <- askExecutionDir
  return $ execDir </> "state.json"

askExecutionDir :: MonadExecution b m => m FilePath
askExecutionDir = do
  planDir <- asks _executionEnvPlanDir
  name <- gets _executionStateName
  return $ planDir </> "execution" </> T.unpack name

writeState :: MonadExecution b m => m ()
writeState = do
  stateFile <- askStateFile
  state <- get
  let encoded = A.encode state
  liftBase $ writeFileFS stateFile encoded

executionRunner :: MonadExecution b m => Task -> m Result
executionRunner task = do
  executionDir <- askExecutionDir
  result <- liftBase $ command executionDir (_taskAction task) (_taskTimeout task)
  state <- get
  let histories = _executionStateHistories state
      latestHistory = History (_taskName task) result
      newHistories = latestHistory : histories
      newState = state { _executionStateHistories = newHistories }
  put newState
  writeState
  return result
  
setup :: MonadExecution b m => m ()
setup = do
  executionDir <- askExecutionDir
  liftBase $ createDirectoryIfMissingFS False executionDir
  input <- asks _executionEnvInput
  let inputCopy = executionDir </> takeBaseName input <.> takeExtension input
  liftBase $ copyFileFS input inputCopy
  writeState

newtype ExecutionT b a = ExecutionT
  { unExecutionT :: ReaderT ExecutionEnv (StateT ExecutionState b) a
  } deriving (Functor, Applicative, Monad, MonadReader ExecutionEnv, MonadState ExecutionState)

-- | Monad boilerplate
instance MonadTrans ExecutionT where
  lift = ExecutionT . lift . lift

-- | Monad boilerplate
instance Monad b => MonadBase b (ExecutionT b) where
  liftBase = lift

instance (MonadCommand b, MonadFS b) => MonadRunner (ExecutionT b) where
  runner = executionRunner

runExecutionT :: Monad b => ExecutionT b a -> ExecutionEnv -> ExecutionState -> b (a, ExecutionState)
runExecutionT (ExecutionT e) env state = runStateT (runReaderT e env) state

listExecutionT :: Monad b => ListT (ExecutionT b) a -> ExecutionEnv -> ExecutionState -> ListT b a
listExecutionT (ListT mStep) exEnv exState = ListT $ do
  (step, exState') <- runExecutionT mStep exEnv exState
  return $ case step of
             Nil -> Nil
             Cons a rest -> Cons a $ listExecutionT rest exEnv exState'

-- hacky, do `start` before yielding elements
consEffect :: Monad m => m () -> ListT m a -> ListT m a
consEffect start (ListT mStep)  = ListT $ do
  start
  mStep

execute :: (MonadCommand b, MonadFS b) => Plan -> ExecutionEnv -> ListT b (Task, Result)
execute plan env = listExecutionT (consEffect setup (unfoldPlan plan)) env state
  where
    name = takeBaseName (_executionEnvInput env)
    state = ExecutionState (T.pack name) []
