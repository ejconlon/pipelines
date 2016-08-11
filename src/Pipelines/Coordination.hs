{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Pipelines.Coordination where

import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Map               as M
import           List.Transformer
import           Pipelines.Core
import           Pipelines.Execution
import           Pipelines.Filesystem

data CoordinationEnv = CoordinationEnv
  { _coordinationEnvBaseDir :: FilePath
  , _coordinationEnvPlans   :: M.Map Name Plan
  } deriving (Show, Eq)

type MonadCoordination b m = (Monad b, MonadWatch b, MonadFS b, Monad m, MonadBase b m,
                              MonadReader CoordinationEnv m)

initializeDirs :: MonadCoordination b m => m ()
initializeDirs = undefined

watch :: MonadCoordination b m => m (Watch b ExecutionEnv)
watch = undefined
