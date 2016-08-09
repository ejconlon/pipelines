{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}

module Pipelines where

-- import Control.Lens
import Control.Monad (forM_)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.UUID (UUID)
import List.Transformer
-- import Options.Applicative
-- import System.FilePath
-- import Turtle

type Name     = String
type Timeout  = String
type Action   = String
type Interval = Int

data Result = OkResult | FailResult deriving (Show, Eq)

data History = History
  { _historyName   :: Name
  , _historyUuid   :: UUID
  , _historyResult :: Result
  } deriving (Show, Eq)

data Task = Task
  { _taskName    :: Name
  , _taskAction  :: Action
  , _taskTimeout :: Interval
  } deriving (Show, Eq)

data Loop
  = StopLoop
  | ContinueLoop
  deriving (Show, Eq)

data Plan = Plan
  { _planTasks   :: [Task]
  , _planLoop    :: Loop
  } deriving (Show, Eq)

data Position =
    StartPos
  | TaskPos Name
  | EndPos
  | FailPos
  deriving (Show, Eq)

data PlanState = PlanState
  { _planStateHistory :: [History]  -- as a stack (most recent first)
  , _planStatePosition :: Position
  } deriving (Show, Eq)

initialPlanState :: PlanState
initialPlanState = PlanState [] StartPos

type MonadPlan b m = (MonadBase b m, MonadReader Plan m, MonadState PlanState m)

lookupTask :: Name -> [Task] -> Maybe Task
lookupTask _ [] = Nothing
lookupTask n (s:ss) | n == _taskName s = Just s
                    | otherwise = lookupTask n ss

headOption :: [a] -> Maybe a
headOption [] = Nothing
headOption (a:_) = Just a

nextTaskAfter :: Name -> [Task] -> Maybe Task
nextTaskAfter _ [] = Nothing
nextTaskAfter _ [_] = Nothing
nextTaskAfter n (s:t:ss) | n == _taskName s = Just t
                         | otherwise = nextTaskAfter n (t:ss)

isEmpty :: MonadPlan IO m => m Bool
isEmpty = null <$> asks _planTasks

isDone :: MonadPlan IO m => m Bool
isDone = (\p -> p == EndPos || p == FailPos) <$> gets _planStatePosition

advancePosition :: MonadPlan IO m => m ()
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
    EndPos -> return ()  -- TODO loop to start
    FailPos -> return ()

-- Advance StartPos to the next task
normalizePosition :: MonadPlan IO m => m ()
normalizePosition = do
  position <- gets _planStatePosition
  case position of
    StartPos -> advancePosition
    _ -> return ()

currentTask :: MonadPlan IO m => m (Maybe Task)
currentTask = do
  tasks <- asks _planTasks
  position <- gets _planStatePosition
  return $ case position of
             StartPos -> Nothing
             TaskPos n -> lookupTask n tasks
             EndPos -> Nothing

type Runner n = Task -> n History

runTask :: MonadPlan IO m => Runner IO -> Task -> m History
runTask runner task = do
  history <- liftBase $ runner task
  let name = _taskName task
      result = _historyResult history
  modify (\s -> s { _planStateHistory = history : (_planStateHistory s) })
  case result of
    FailResult -> modify (\s -> s { _planStatePosition = FailPos })
    OkResult -> advancePosition
  return history

step :: MonadPlan IO m => Runner IO -> m (Maybe History)
step runner = do
  normalizePosition
  task <- currentTask
  forM task $ runTask runner

type Evaluator m n = forall a. m a -> Plan -> PlanState -> n (a, PlanState)

subWalk :: MonadPlan IO m => Runner IO -> Evaluator m IO -> Plan -> PlanState -> IO (Step IO History)
subWalk runner evaluator plan planState = do
  (done, state') <- evaluator isDone plan planState
  if done
    then return Nil
    else do
      (mh, state'') <- evaluator (step runner) plan state'
      case mh of
        Just h -> return $ Cons h (walk runner evaluator plan state'')
        Nothing -> return Nil

walk :: MonadPlan IO m => Runner IO -> Evaluator m IO -> Plan -> PlanState -> ListT IO History
walk runner evaluator plan planState = ListT $ subWalk runner evaluator plan planState

newtype PlanT a = PlanT
  { unPlanT :: ReaderT Plan (StateT PlanState IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Plan,
            MonadState PlanState, MonadIO, MonadBase IO)

-- runPlanT :: PlanT a -> Plan -> PlanState -> IO (a, PlanState)
runPlanT :: Evaluator PlanT IO
runPlanT (PlanT x) = runStateT . runReaderT x

unfoldPlan :: Runner IO -> Plan -> ListT IO History
unfoldPlan runner plan = walk runner runPlanT plan initialPlanState
