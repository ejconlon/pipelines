-- | Utilities to define and run pipelines of tasks.
module Pipelines.Core
  ( unfoldPlan
  ) where

import           Control.Exception
import           Control.Monad          (forM_, unless)
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson             ((.:), (.=))
import qualified Data.Aeson             as A
import qualified Data.Aeson.Types       as A
import qualified Data.Text              as T
import           List.Transformer
import           Pipelines.Common
import           Pipelines.Types

-- | Our position in a Plan's list of Tasks
data Position =
    StartPos
  | TaskPos Name
  | EndPos
  | FailPos
  deriving (Show, Eq)

-- | State relevant to the execution of a task
data PlanState = PlanState
  { _planStatePosition :: Position
  } deriving (Show, Eq)

-- | The default plan state: no history and at starting position
initialPlanState :: PlanState
initialPlanState = PlanState StartPos

-- | A typeclass to wrangle our Plan operations
type MonadPlan b m = (MonadRunner b, MonadBase b m,
                      MonadReader Plan m, MonadState PlanState m)

-- | Finds a Task by name
lookupTask :: Name -> [Task] -> Maybe Task
lookupTask _ [] = Nothing
lookupTask n (s:ss) | n == _taskName s = Just s
                    | otherwise = lookupTask n ss

-- | Don't need to import safe, so we'll just define this here -_-
headOption :: [a] -> Maybe a
headOption [] = Nothing
headOption (a:_) = Just a

-- | Finds the next task
nextTaskAfter :: Name -> [Task] -> Maybe Task
nextTaskAfter _ [] = Nothing
nextTaskAfter _ [_] = Nothing
nextTaskAfter n (s:t:ss) | n == _taskName s = Just t
                         | otherwise = nextTaskAfter n (t:ss)

-- | Are there any tasks in this Plan?
isEmpty :: MonadPlan b m => m Bool
isEmpty = null <$> asks _planTasks

-- | Is this Plan failed or finished?
isDone :: MonadPlan b m => m Bool
isDone = (\p -> p == EndPos || p == FailPos) <$> gets _planStatePosition

-- | Updates our state to point to the next Task to be run
advancePosition :: MonadPlan b m => m ()
advancePosition = do
  tasks <- asks _planTasks
  loop <- asks _planLoop
  position <- gets _planStatePosition
  case position of
    StartPos -> do
      let first = headOption tasks
          pos = maybe EndPos (TaskPos . _taskName) first
      modify' (\s -> s { _planStatePosition = pos })
    TaskPos n -> do
      let next = nextTaskAfter n tasks
          pos = maybe EndPos (TaskPos . _taskName) next
      modify (\s -> s { _planStatePosition = pos })
    EndPos ->
      case loop of
        StopLoop -> return ()
        ContinueLoop -> do
          empty <- isEmpty
          unless empty $
            modify (\s -> s { _planStatePosition = StartPos }) >> advancePosition
    FailPos -> return ()

-- | Handles default state: advances StartPos to the next task
normalizePosition :: MonadPlan b m => m ()
normalizePosition = do
  position <- gets _planStatePosition
  case position of
    StartPos -> advancePosition
    _ -> return ()

-- | Finds the current task
currentTask :: MonadPlan b m => m (Maybe Task)
currentTask = do
  tasks <- asks _planTasks
  position <- gets _planStatePosition
  return $ case position of
             StartPos -> Nothing
             TaskPos n -> lookupTask n tasks
             EndPos -> Nothing

-- | Runs the given Task and updates state
runTask :: MonadPlan b m => Task -> m (Name, Result)
runTask task = do
  result <- liftBase $ runner task
  case result of
    FailResult -> modify (\s -> s { _planStatePosition = FailPos })
    OkResult -> advancePosition
  return (_taskName task, result)

-- | Runs the next Task and updates state
step :: MonadPlan b m => m (Maybe (Name, Result))
step = do
  normalizePosition
  task <- currentTask
  forM task runTask

-- | A list of Task-execution actions
walk :: MonadPlan b m => ListT m (Name, Result)
walk = ListT $ do
  done <- isDone
  if done
    then return Nil
    else do
      mh <- step
      case mh of
        Just h -> return $ Cons h walk
        Nothing -> return Nil

-- | A concrete interpretation of `MonadPlan`
newtype PlanT b a = PlanT
  { unPlanT :: ReaderT Plan (StateT PlanState b) a }
  deriving (Functor, Applicative, Monad, MonadReader Plan,
            MonadState PlanState)

-- | Monad boilerplate
instance MonadTrans PlanT where
  lift = PlanT . lift . lift

-- | Monad boilerplate
instance Monad b => MonadBase b (PlanT b) where
  liftBase = lift

-- | Monad boilerplate
runPlanT :: PlanT b a -> Plan -> PlanState -> b (a, PlanState)
runPlanT (PlanT x) = runStateT . runReaderT x

-- | Projects `PlanT b` actions in the list transformer to the base monad
listPlanT :: Monad b => ListT (PlanT b) a -> Plan -> PlanState -> ListT b a
listPlanT (ListT mStep) plan state = ListT $ do
  (step, state') <- runPlanT mStep plan state
  return $ case step of
             Nil -> Nil
             Cons a rest -> Cons a $ listPlanT rest plan state'

-- | Unfolds a Plan into a sequence of Task-execution actions that yield History
unfoldPlan :: MonadRunner b => Plan -> ListT b (Name, Result)
unfoldPlan plan = listPlanT walk plan initialPlanState
