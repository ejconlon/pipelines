{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad.Catch
import           Control.Monad.Catch.Pure
import           Control.Monad.Reader
import           Data.Functor.Identity
import qualified Data.Map.Strict          as M
import qualified Data.Text                as T
import           Data.Time.Clock
import           Data.Typeable
import           Pipelines
import           Test.Tasty
import           Test.Tasty.HUnit

-- Utilities to work with exception equality:

isOk :: (Eq a, Show a) => Either SomeException a -> a -> Assertion
isOk (Left e) _ = fail $ "got fail " ++ show e
isOk (Right x) a = x @?= a

isFail :: (Show a, Exception e, Eq e) => Either SomeException a -> e -> Assertion
isFail (Right x) _ = fail $ "got ok " ++ show x
isFail (Left z) e =
  case cast z of
    Nothing -> fail $ "incompatible: " ++ show z
    Just f -> f @?= e

-- Start out by defining our ExpectRunner Monad that implements MonadRunner:

data ExpectConfig = ExpectConfig
  { _expectConfigLookup :: Action -> Maybe Result
  }

-- Errors in the test suite, NOT errors in the system under test.
-- Task errors should be configured in ExpectConfig.
data NotFoundException = NotFoundException Name deriving (Show, Eq, Typeable)
instance Exception NotFoundException where

type MonadExpect m = (Monad m, MonadReader ExpectConfig m, MonadThrow m)

newtype ExpectRunner a = ExpectRunner
  { unExpectRunner :: ReaderT ExpectConfig (CatchT Identity) a
  } deriving (Functor, Applicative, Monad,
              MonadReader ExpectConfig, MonadThrow)

runExpect :: ExpectRunner a -> ExpectConfig -> Either SomeException a
runExpect (ExpectRunner r) c = runIdentity $ runCatchT $ runReaderT r c

expectRunner :: MonadExpect m => Task -> m Result
expectRunner task = do
  let name = _taskName task
  look <- asks _expectConfigLookup
  case look name of
    Nothing -> throwM (NotFoundException name)
    Just r -> return r

instance MonadRunner ExpectRunner where
  data Uid ExpectRunner = Unit
  runner task Unit = expectRunner task

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
      expected =
        [ ("taskA", OkResult)
        , ("taskB", OkResult)
        , ("taskC", OkResult)
        ]
      taken = takeAllPlan simplePlan Unit
      actual = runExpect taken config
  actual `isOk` expected

startTime :: UTCTime
startTime = UTCTime (toEnum 1) (fromIntegral 2)

runFS :: FakeFST (CatchT Identity) a -> FSDir -> Either SomeException (a, FSDir, [WatchEvent])
runFS fst dir = runIdentity $ runCatchT $ runFakeFST fst startTime dir 

testFSRootExists :: TestTree
testFSRootExists = testCase "fs root exists" $ do
  runFS (doesDirectoryExistFS "/") emptyFSDir `isOk` (True, emptyFSDir, [])

testFS :: TestTree
testFS = testGroup "fs"
  [ testFSRootExists
  ]

tests :: TestTree
tests = testGroup "tests"
  [ testSimple
  , testFS  
  ]

main :: IO ()
main = defaultMain tests
