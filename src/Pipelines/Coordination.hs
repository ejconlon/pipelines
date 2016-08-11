{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Pipelines.Coordination where

import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Map as M
import Pipelines.Core
import qualified System.FSNotify as N

data CoordinationEnv = CoordinationEnv
  { _coordinationEnvBaseDir :: FilePath
  , _coordinationEnvPlans   :: M.Map String Plan
  } deriving (Show, Eq)

type MonadCoordination b m = (Monad b, MonadIO b, Monad m, MonadIO m, MonadBase b m,
                              MonadReader CoordinationEnv m)

initializeDirs :: MonadCoordination b m => m ()
initializeDirs = undefined

watchDirs :: MonadCoordination b m => m ()
watchDirs = undefined
