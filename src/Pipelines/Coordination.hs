module Pipelines.Coordination
  ( CoordinationEnv(..)
  , coordinate
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
  , _coordinationEnvPlan    :: Plan
  } deriving (Show, Eq)

type MonadCoordination b m = (Monad b, MonadWatch b, MonadFS b, Monad m, MonadBase b m,
                              MonadReader CoordinationEnv m)

initializeDirs :: MonadCoordination b m => m ()
initializeDirs = do
  baseDir <- asks _coordinationEnvBaseDir
  liftBase $ createDirectoryIfMissingFS True baseDir
  plan <- asks _coordinationEnvPlan
  let planDir = (baseDir </>) . T.unpack . _planName $ plan
  liftBase $ createDirectoryIfMissingFS False planDir
  forM_ ["input", "state", "tasks", "archive"] $ \subName ->
    liftBase $ createDirectoryIfMissingFS False $ planDir </> subName

watch :: MonadCoordination b m => m (Watch b (Plan, ExecutionEnv))
watch = do
  baseDir <- asks _coordinationEnvBaseDir
  plan <- asks _coordinationEnvPlan
  let planName = _planName plan
      planDir = baseDir </> T.unpack planName
      planInputsDir = planDir </> "input"
  watch <- liftBase $ watchDir planInputsDir (const True)
  return $ (\e -> (plan, ExecutionEnv planDir (_watchEventPath e))) <$> watch

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

coordinate :: (MonadFS b, MonadWatch b, MonadThrow b) => FilePath -> Plan -> b (Watch b (Plan, ExecutionEnv))
coordinate baseDir plan = do
  let env = CoordinationEnv baseDir plan
      action = initializeDirs >> watch
  runCoordinationT action env
