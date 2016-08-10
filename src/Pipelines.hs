{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}

module Pipelines
  ( Name
  , Timeout
  , Action
  , Interval
  , Result(..)
  , History(..)
  , Task(..)
  , Loop(..)
  , Plan(..)
  , unfoldPlan
  , takePlan
  ) where

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
  { _planName  :: Name
  , _planTasks :: [Task]
  , _planLoop  :: Loop
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

class Monad b => MonadRunner b where
  -- Given a plan name and a stack of task results,
  -- runs a plan and returns a result in context
  runner :: Name -> [History] -> Task -> b History

type MonadPlan b m = (MonadRunner b, MonadBase b m,
                      MonadReader Plan m, MonadState PlanState m)

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

isEmpty :: MonadPlan b m => m Bool
isEmpty = null <$> asks _planTasks

isDone :: MonadPlan b m => m Bool
isDone = (\p -> p == EndPos || p == FailPos) <$> gets _planStatePosition

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
    EndPos -> return ()  -- TODO loop to start
    FailPos -> return ()

-- Advance StartPos to the next task
normalizePosition :: MonadPlan b m => m ()
normalizePosition = do
  position <- gets _planStatePosition
  case position of
    StartPos -> advancePosition
    _ -> return ()

currentTask :: MonadPlan b m => m (Maybe Task)
currentTask = do
  tasks <- asks _planTasks
  position <- gets _planStatePosition
  return $ case position of
             StartPos -> Nothing
             TaskPos n -> lookupTask n tasks
             EndPos -> Nothing

type Runner b = Task -> b History

runTask :: MonadPlan b m => Task -> m History
runTask task = do
  name <- asks _planName
  recentHistory <- gets _planStateHistory
  history <- liftBase $ runner name recentHistory task
  modify (\s -> s { _planStateHistory = history : (_planStateHistory s) })
  case _historyResult history of
    FailResult -> modify (\s -> s { _planStatePosition = FailPos })
    OkResult -> advancePosition
  return history

step :: MonadPlan b m => m (Maybe History)
step = do
  normalizePosition
  task <- currentTask
  forM task runTask

walk :: MonadPlan b m => ListT m History
walk = ListT $ do
  done <- isDone
  if done
    then return Nil
    else do
      mh <- step
      case mh of
        Just h -> return $ Cons h walk
        Nothing -> return Nil

newtype PlanT b a = PlanT
  { unPlanT :: ReaderT Plan (StateT PlanState b) a }
  deriving (Functor, Applicative, Monad, MonadReader Plan,
            MonadState PlanState)

instance MonadTrans PlanT where
  lift = PlanT . lift . lift

instance Monad b => MonadBase b (PlanT b) where
  liftBase = lift

type Evaluator b m = forall a. m a -> Plan -> PlanState -> b (a, PlanState)

-- runPlanT :: PlanT b a -> Plan -> PlanState -> b (a, PlanState)
runPlanT :: Monad b => Evaluator b (PlanT b)
runPlanT (PlanT x) = runStateT . runReaderT x

listPlanT :: Monad b => ListT (PlanT b) a -> Plan -> PlanState -> ListT b a
listPlanT (ListT mStep) plan state = ListT $ do
  (step, state') <- runPlanT mStep plan state
  return $ case step of
             Nil -> Nil
             Cons a rest -> Cons a $ listPlanT rest plan state'

unfoldPlan :: MonadRunner b => Plan -> ListT b History
unfoldPlan plan = listPlanT walk plan initialPlanState

takeListT :: Monad b => Int -> ListT b a -> b [a]
takeListT = undefined -- TODO

-- Take at most `n` elements from the plan (blocks and returns all `n` at once)
takePlan :: MonadRunner b => Int -> Plan -> b [History]
takePlan n plan = takeListT n $ unfoldPlan plan
