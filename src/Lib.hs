{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Lib
    ( someFunc
    ) where

-- import Control.Lens
import Control.Monad (forM_)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Loops (untilM)
import Control.Monad.Reader
import Control.Monad.State
import Data.Conduit
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
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
  , _planWorkdir :: FilePath
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

type MonadPlan m = (MonadIO m, MonadCatch m, MonadReader Plan m, MonadState PlanState m)

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

isEmpty :: MonadPlan m => m Bool
isEmpty = null <$> asks _planTasks

isDone :: MonadPlan m => m Bool
isDone = (\p -> p == EndPos || p == FailPos) <$> gets _planStatePosition

advancePosition :: MonadPlan m => m ()
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
normalizePosition :: MonadPlan m => m ()
normalizePosition = do
  position <- gets _planStatePosition
  case position of
    StartPos -> advancePosition
    _ -> return ()

currentTask :: MonadPlan m => m (Maybe Task)
currentTask = do
  tasks <- asks _planTasks
  position <- gets _planStatePosition
  return $ case position of
             StartPos -> Nothing
             TaskPos n -> lookupTask n tasks
             EndPos -> Nothing

runTaskIO :: Task -> FilePath -> UUID -> IO Result
runTaskIO = undefined

runTask :: MonadPlan m => Task -> m History
runTask task = do
  workdir <- asks _planWorkdir
  uuid <- liftIO nextRandom
  result <- liftIO $ runTaskIO task workdir uuid
  let name = _taskName task
      history = History name uuid result
  modify (\s -> s { _planStateHistory = history : (_planStateHistory s) })
  case result of
    FailResult -> modify (\s -> s { _planStatePosition = FailPos })
    OkResult -> advancePosition
  return history

newtype ConcretePlan a = ConcretePlan
  { unConcretePlan :: ReaderT Plan (StateT PlanState IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Plan,
            MonadState PlanState, MonadIO, MonadThrow, MonadCatch)

runConcretePlan :: ConcretePlan a -> Plan -> PlanState -> IO (a, PlanState)
runConcretePlan (ConcretePlan x) = runStateT . runReaderT x

-- unfoldPlan :: Plan -> Source IO History
-- TODO

step :: MonadPlan m => m (Maybe History)
step = do
  normalizePosition
  task <- currentTask
  forM task runTask

someFunc :: IO ()
someFunc = putStrLn "someFunc"
