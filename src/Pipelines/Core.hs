{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Utilities to define and run pipelines of tasks.
module Pipelines.Core
  ( MonadRunner(..)
  , Name
  , Action
  , Interval
  , Result(..)
  , Task(..)
  , Loop(..)
  , Plan(..)
  , unfoldPlan
  , takePlan
  , takeAllPlan
  , executePlan
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

-- | A stringy identifier for a Plan or a Task
type Name = T.Text

-- | A stringy identifier for a Task's action in the world
type Action = T.Text

-- | A duration for which we can set Task timeouts
type Interval = Int

-- | A named unit of work
data Task = Task
  { _taskName    :: Name
  , _taskAction  :: Action
  , _taskTimeout :: Interval
  } deriving (Show, Eq)

instance A.FromJSON Task where
  parseJSON (A.Object m) =
    Task <$>
      m .: "name"    <*>
      m .: "action"  <*>
      m .: "timeout"
  parseJSON invalid = A.typeMismatch "Task" invalid

instance A.ToJSON Task where
  toJSON (Task name action timeout) = A.object
    [ "name" .= name
    , "action" .= action
    , "timeout" .= timeout
    ]

-- | Indicates whether a Plan should loop forever or stop
data Loop
  = StopLoop
  | ContinueLoop
  deriving (Show, Eq)

instance A.FromJSON Loop where
  parseJSON (A.String t) =
    case t of
      "stop" -> return StopLoop
      "continue" -> return ContinueLoop
      _ -> fail ("invalid Loop " ++ T.unpack t)
  parseJSON invalid = A.typeMismatch "Loop" invalid

instance A.ToJSON Loop where
  toJSON StopLoop = A.String "stop"
  toJSON ContinueLoop = A.String "continue"

-- | A sequence of tasks
data Plan = Plan
  { _planName  :: Name
  , _planTasks :: [Task]
  , _planLoop  :: Loop
  } deriving (Show, Eq)

instance A.FromJSON Plan where
  parseJSON (A.Object m) =
    Plan <$>
      m .: "name"  <*>
      m .: "tasks" <*>
      m .: "loop"
  parseJSON invalid = A.typeMismatch "Plan" invalid

instance A.ToJSON Plan where
  toJSON (Plan name tasks loop) = A.object
    [ "name" .= name
    , "tasks" .= tasks
    , "loop" .= loop
    ]

-- | The Result of running a Task
-- TODO(eric) possibly a retry state
data Result = OkResult | FailResult deriving (Show, Eq)

instance A.FromJSON Result where
  parseJSON (A.String t) =
    case t of
      "ok" -> return OkResult
      "fail" -> return FailResult
      _ -> fail ("invalid Result " ++ T.unpack t)
  parseJSON invalid = A.typeMismatch "Result" invalid

instance A.ToJSON Result where
  toJSON OkResult = A.String "ok"
  toJSON FailResult = A.String "fail"

-- | Our position in a Plan's list of Tasks
data Position =
    StartPos
  | TaskPos Name
  | EndPos
  | FailPos
  deriving (Show, Eq)

data PlanEnv b = PlanEnv
  { _planEnvPlan :: Plan
  , _planEnvUid  :: Uid b
  }

deriving instance Show (Uid b) => Show (PlanEnv b)
deriving instance Eq (Uid b) => Eq (PlanEnv b)

-- | State relevant to the execution of a task
data PlanState = PlanState
  { _planStatePosition :: Position
  } deriving (Show, Eq)

-- | The default plan state: no history and at starting position
initialPlanState :: PlanState
initialPlanState = PlanState StartPos

-- | The thing that actually runs tasks.
-- Given a plan name and a stack of task results,
-- runs a plan and returns a result in context.
-- Newtype this to control how tasks are run:
-- A real implementation might work in IO over the filesystem.
-- An implementation for tests might work over State and yield fake history.
class Monad b => MonadRunner b where
  data Uid b :: *
  runner :: Plan -> Task -> Uid b -> b Result
  
-- | A typeclass to wrangle our Plan operations
type MonadPlan b m = (MonadRunner b, MonadBase b m,
                      MonadReader (PlanEnv b) m, MonadState PlanState m)

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
isEmpty = null <$> asks (_planTasks . _planEnvPlan)

-- | Is this Plan failed or finished?
isDone :: MonadPlan b m => m Bool
isDone = (\p -> p == EndPos || p == FailPos) <$> gets _planStatePosition

-- | Updates our state to point to the next Task to be run
advancePosition :: MonadPlan b m => m ()
advancePosition = do
  tasks <- asks $ _planTasks . _planEnvPlan
  loop <- asks $ _planLoop . _planEnvPlan
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
  tasks <- asks $ _planTasks . _planEnvPlan
  position <- gets _planStatePosition
  return $ case position of
             StartPos -> Nothing
             TaskPos n -> lookupTask n tasks
             EndPos -> Nothing

-- | Runs the given Task and updates state
runTask :: MonadPlan b m => Task -> m (Name, Result)
runTask task = do
  plan <- asks _planEnvPlan
  uid <- asks _planEnvUid
  result <- liftBase $ runner plan task uid
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
  { unPlanT :: ReaderT (PlanEnv b) (StateT PlanState b) a }
  deriving (Functor, Applicative, Monad, MonadReader (PlanEnv b),
            MonadState PlanState)

-- | Monad boilerplate
instance MonadTrans PlanT where
  lift = PlanT . lift . lift

-- | Monad boilerplate
instance Monad b => MonadBase b (PlanT b) where
  liftBase = lift

-- | Monad boilerplate
runPlanT :: PlanT b a -> PlanEnv b -> PlanState -> b (a, PlanState)
runPlanT (PlanT x) = runStateT . runReaderT x

-- | Projects `PlanT b` actions in the list transformer to the base monad
listPlanT :: Monad b => ListT (PlanT b) a -> PlanEnv b -> PlanState -> ListT b a
listPlanT (ListT mStep) planEnv state = ListT $ do
  (step, state') <- runPlanT mStep planEnv state
  return $ case step of
             Nil -> Nil
             Cons a rest -> Cons a $ listPlanT rest planEnv state'

-- | Unfolds a Plan into a sequence of Task-execution actions that yield History
unfoldPlan :: MonadRunner b => Plan -> Uid b -> ListT b (Name, Result)
unfoldPlan plan uid = listPlanT walk (PlanEnv plan uid) initialPlanState

-- | Takes at most `n` elements from the ListT (blocks and returns all `n` at once)
takeListT :: Monad b => Int -> ListT b a -> b [a]
takeListT n (ListT mStep) =
  if n <= 0
    then return []
    else do
      step <- mStep
      case step of
        Nil -> return []
        Cons a rest -> (a:) <$> takeListT (n - 1) rest

-- | Takes at most `n` elements from the Plan execution (blocks and returns all `n` at once)
takePlan :: MonadRunner b => Int -> Plan -> Uid b -> b [(Name, Result)]
takePlan n plan uid = takeListT n $ unfoldPlan plan uid

-- | Takes all elements from the ListT (blocks and returns all at once, if at all)
takeAllListT :: Monad b => ListT b a -> b [a]
takeAllListT (ListT mStep) = do
  step <- mStep
  case step of
    Nil -> return []
    Cons a rest -> (a:) <$> takeAllListT rest

-- | Takes all elements from the Plan execution (blocks and returns all at once, if at all)
takeAllPlan :: MonadRunner b => Plan -> Uid b -> b [(Name, Result)]
takeAllPlan plan uid = takeAllListT $ unfoldPlan plan uid

-- | Executes a Plan, discarding the result. May never return.
executePlan :: MonadRunner b => Plan -> Uid b -> b ()
executePlan plan uid = runListT $ unfoldPlan plan uid
