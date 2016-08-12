{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Pipelines.Coordination
  ( coordinate
  ) where

import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Text              as T
import           List.Transformer
import           Pipelines.Common
import           Pipelines.Execution
import           Pipelines.Filesystem
import           Pipelines.Types
import           System.FilePath

data CoordinationEnv = CoordinationEnv
  { _coordinationEnvBaseDir :: FilePath
  , _coordinationEnvPlans   :: [Plan]
  } deriving (Show, Eq)

type MonadCoordination b m = (Monad b, MonadWatch b, MonadFS b, Monad m, MonadBase b m,
                              MonadReader CoordinationEnv m)

initializeDirs :: MonadCoordination b m => m ()
initializeDirs = do
  baseDir <- asks _coordinationEnvBaseDir
  liftBase $ createDirectoryIfMissingFS True baseDir
  plans <- asks _coordinationEnvPlans
  let planDirs = (baseDir </>) . T.unpack . _planName <$> plans
  forM_ planDirs $ \planDir -> do
    liftBase $ createDirectoryIfMissingFS False planDir
    forM_ ["input", "state", "tasks", "archive"] $ \subName ->
      liftBase $ createDirectoryIfMissingFS False $ planDir </> subName

watch :: MonadCoordination b m => m (Watch b (Plan, ExecutionEnv))
watch = do
  baseDir <- asks _coordinationEnvBaseDir
  plans <- asks _coordinationEnvPlans
  watches <- forM plans $ \plan -> do
    let planName = _planName plan
        planDir = baseDir </> T.unpack planName
        planInputsDir = planDir </> "input"
    liftBase $ watchDir planInputsDir (const True) (\e -> (plan, ExecutionEnv planDir (_watchEventPath e)))
  return $ mconcat watches

newtype CoordinationT b a = CoordinationT
  { unCoordinationT :: ReaderT CoordinationEnv b a
  } deriving (Functor, Applicative, Monad, MonadReader CoordinationEnv)

-- | Monad boilerplate
instance MonadTrans CoordinationT where
  lift = CoordinationT . lift

-- | Monad boilerplate
instance Monad b => MonadBase b (CoordinationT b) where
  liftBase = lift

runCoordinationT :: Monad b => CoordinationT b a -> CoordinationEnv -> b a
runCoordinationT (CoordinationT x) = runReaderT x

coordinate :: (MonadFS b, MonadWatch b, MonadThrow b) => FilePath -> [Plan] -> b (Watch b (Plan, ExecutionEnv))
coordinate path plans = do
  let env = CoordinationEnv path plans
      action = initializeDirs >> watch
  runCoordinationT action env
