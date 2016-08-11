{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Pipelines.Execution where

import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson             ((.:), (.=))
import qualified Data.Aeson             as A
import qualified Data.Aeson.Types       as A
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Set               as S
import qualified Data.Text              as T
import           List.Transformer
import           Pipelines.Core
import           Pipelines.Filesystem
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

type MonadExecution b m = (Monad b, MonadFS b, Monad m, MonadBase b m,
                           MonadReader ExecutionEnv m)

initializeSubdirs :: MonadExecution b m => m ()
initializeSubdirs = undefined

asksName :: MonadExecution b m => m Name
asksName = do
  input <- asks _executionEnvInput
  return $ T.pack $ takeBaseName input

askStateFile :: MonadExecution b m => m FilePath
askStateFile = do
  planDir <- asks _executionEnvPlanDir
  name <- asksName
  return $ planDir </> "state" </> T.unpack name </> ".json"

askTaskBaseDir :: MonadExecution b m => m FilePath
askTaskBaseDir = do
  planDir <- asks _executionEnvPlanDir
  return $ planDir </> "tasks"

askTaskDir :: MonadExecution b m => Task -> m FilePath
askTaskDir task = do
  taskBase <- askTaskBaseDir
  name <- asksName
  return $ taskBase </> T.unpack name

askArchiveFile :: MonadExecution b m => m FilePath
askArchiveFile = do
  planDir <- asks _executionEnvPlanDir
  input <- asks _executionEnvInput
  return $ replaceDirectory input $ planDir </> "archive"

writeState :: MonadExecution b m => ExecutionState -> m ()
writeState state = do
  stateFile <- askStateFile
  let encoded = A.encode state
  liftBase $ writeFileFS stateFile encoded

readState :: MonadExecution b m => m (Maybe ExecutionState)
readState = do
  stateFile <- askStateFile
  exists <- liftBase $ doesFileExistFS stateFile
  if exists
    then A.decode <$> liftBase (readFileFS stateFile)
    else return Nothing

newtype ExecutionT b a = ExecutionT
  { unExecutionT :: ReaderT ExecutionEnv b a
  } deriving (Functor, Applicative, Monad, MonadReader ExecutionEnv, MonadThrow, MonadCatch)

-- | Monad boilerplate
instance MonadTrans ExecutionT where
  lift = ExecutionT . lift

-- | Monad boilerplate
instance Monad b => MonadBase b (ExecutionT b) where
  liftBase = lift

instance MonadRunner b => MonadRunner (ExecutionT b) where
  data Uid (ExecutionT b) = Wrap (Uid b)
  runner task (Wrap uid) = lift (runner task uid)

runExecutionT :: ExecutionT b a -> ExecutionEnv -> b a
runExecutionT (ExecutionT e) = runReaderT e
