{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Pipelines.Coordination
  ( CoordinationEnv(..)
  , MonadCoordination
  , initializeDirs
  , watch  
  ) where

import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import List.Transformer
import Pipelines.Core
import Pipelines.Execution
import Pipelines.Filesystem
import System.FilePath

data CoordinationEnv = CoordinationEnv
  { _coordinationEnvBaseDir :: FilePath
  , _coordinationEnvPlans   :: M.Map Name Plan
  } deriving (Show, Eq)

type MonadCoordination b m = (Monad b, MonadWatch b, MonadFS b, Monad m, MonadBase b m,
                              MonadReader CoordinationEnv m)

initializeDirs :: MonadCoordination b m => m ()
initializeDirs = do
  baseDir <- asks _coordinationEnvBaseDir
  liftBase $ createDirectoryIfMissingFS True baseDir
  plans <- asks _coordinationEnvPlans
  let planDirs = (baseDir </>) . T.unpack <$> M.keys plans
  forM_ planDirs $ \planDir -> do
    liftBase $ createDirectoryIfMissingFS False planDir
    forM_ ["input", "state", "tasks", "archive"] $ \subName -> do
      liftBase $ createDirectoryIfMissingFS False $ planDir </> subName                                                  

watch :: MonadCoordination b m => m (Watch b ExecutionEnv)
watch = undefined
