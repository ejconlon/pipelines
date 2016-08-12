{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pipelines.Coordination where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Text as T
import List.Transformer
import Pipelines.Core
import Pipelines.Execution
import Pipelines.Filesystem
import System.FilePath

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
    forM_ ["input", "state", "tasks", "archive"] $ \subName -> do
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

-- | Fair >>=
class Monad b => MonadDiagonal b where
  diagonal :: ListT b x -> (x -> ListT b y) -> ListT b y
  -- interleave :: ListT b x -> ListT b x -> ListT b x
  -- interleaveAll :: [ListT b x] -> ListT b x

instance MonadDiagonal IO where
  diagonal l f = l >>= f  -- TODO there must be a fair version of this

coordinate :: (MonadFS b, MonadWatch b, MonadThrow b) => FilePath -> [Plan] -> b (Watch b (Plan, ExecutionEnv))
coordinate path plans = do
  let env = CoordinationEnv path plans
      action = initializeDirs >> watch
  runCoordinationT action env
