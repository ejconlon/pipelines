{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipelines.Watcher where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.IO.Class
import qualified Data.Set as S
import qualified Data.Text as T
import List.Transformer
import Pipelines.Core
import System.Directory
import System.FilePath

data ExecutionEnv = ExecutionEnv
  { _executionEnvPlan    :: Plan
  , _executionEnvBaseDir :: FilePath
  , _executionEnvName    :: Name
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

type MonadExecution m = (Monad m, MonadIO m, MonadReader ExecutionEnv m, MonadRunner m, MonadThrow m)

askStateFile :: MonadReader ExecutionEnv m => m FilePath
askStateFile = do
  base <- asks _executionEnvBaseDir
  name <- asks _executionEnvName
  return $ base </> "state" </> T.unpack name </> ".json"

askTaskBaseDir :: MonadReader ExecutionEnv m => m FilePath
askTaskBaseDir = do
  base <- asks _executionEnvBaseDir
  return $ base </> "tasks"

askTaskDir :: MonadReader ExecutionEnv m => Task -> m FilePath
askTaskDir task = do
  taskBase <- askTaskBaseDir
  name <- asks _executionEnvName
  return $ taskBase </> T.unpack name

askArchiveFile :: MonadReader ExecutionEnv m => m FilePath
askArchiveFile = do
  base <- asks _executionEnvBaseDir
  input <- asks _executionEnvInput
  return $ replaceDirectory input $ base </> "archive"

writeState :: MonadExecution m => ExecutionState -> m ()
writeState state = do
  stateFile <- askStateFile
  let encoded = A.encode state
  liftIO $ BL.writeFile stateFile encoded

readState :: MonadExecution m => m (Maybe ExecutionState)
readState = do
  stateFile <- askStateFile
  exists <- liftIO $ doesFileExist stateFile
  if exists
    then A.decode <$> (liftIO (BL.readFile stateFile))
    else return Nothing
