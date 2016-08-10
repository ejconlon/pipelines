{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor.Identity
import qualified Data.Map.Strict       as M
import qualified Data.Text             as T
import           Pipelines
import           Test.Tasty
import           Test.Tasty.HUnit

-- Start out by defining our ExpectRunner Monad that implements MonadRunner:

data ExpectConfig = ExpectConfig
  { _expectConfigLookup :: Action -> Maybe Result
  }

-- Errors in the test suite, NOT errors in the system under test.
-- Task errors should be configured in ExpectConfig.
data ExpectError =
  NotFoundError Name
  deriving (Show, Eq)

data ExpectState = ExpectState
  { _expectStateId :: Int
  } deriving (Show, Eq)

initialExpectState :: ExpectState
initialExpectState = ExpectState 0

type MonadExpect m = (Monad m, MonadReader ExpectConfig m,
                      MonadState ExpectState m, MonadError ExpectError m)

newtype ExpectRunner a = ExpectRunner
  { unExpectRunner :: ReaderT ExpectConfig (ExceptT ExpectError (StateT ExpectState Identity)) a
  } deriving (Functor, Applicative, Monad,
              MonadReader ExpectConfig, MonadState ExpectState, MonadError ExpectError)

runExpect :: ExpectRunner a -> ExpectConfig -> Either ExpectError a
runExpect (ExpectRunner r) c =
  runIdentity (evalStateT (runExceptT (runReaderT r c)) initialExpectState)

expectHistory :: MonadExpect m => Name -> Result -> m History
expectHistory name result = do
  curId <- gets _expectStateId
  modify (\s -> s { _expectStateId = _expectStateId s + 1 })
  return $ History name (T.pack (show curId)) result

expectRunner :: MonadExpect m => Plan -> [History] -> Task -> m History
expectRunner _ _ task = do
  let name = _taskName task
  look <- asks _expectConfigLookup
  case look name of
    Nothing -> throwError $ NotFoundError name
    Just result -> expectHistory name result

instance MonadRunner ExpectRunner where
  runner = expectRunner

-- | A simple 3-task plan
simplePlan :: Plan
simplePlan = Plan
  { _planName = "simplePlan"
  , _planTasks = [ Task "taskA" "actionA" 1
                 , Task "taskB" "actionB" 2
                 , Task "taskC" "actionC" 3
                 ]
  , _planLoop = StopLoop
  }

-- | Assert we can successfully run our 3 tasks
testSimple :: TestTree
testSimple = testCase "simple" $ do
  let look _ = Just OkResult
      config = ExpectConfig look
      expected = Right
        [ History "taskA" "0" OkResult
        , History "taskB" "1" OkResult
        , History "taskC" "2" OkResult
        ]
      taken = takeAllPlan simplePlan
      actual = runExpect taken config
  actual @?= expected

tests :: TestTree
tests = testGroup "tests"
  [ testSimple
  ]

main :: IO ()
main = defaultMain tests
