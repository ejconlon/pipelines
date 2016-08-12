{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

import qualified Data.ByteString.Lazy.Char8 as BLC
import           Control.Monad.Catch
import           Control.Monad.Catch.Pure
import           Control.Monad.Reader
import           Data.Functor.Identity
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import           Data.Time.Clock
import           Data.Typeable
import           Pipelines
import           Test.Tasty
import           Test.Tasty.HUnit

-- Utilities to work with exception equality:

isOkLike :: (Eq a, Show a) => Either SomeException a -> (a -> Assertion) -> Assertion
isOkLike (Left e) _ = fail $ "got fail " ++ show e
isOkLike (Right x) f = f x

isOk :: (Eq a, Show a) => Either SomeException a -> a -> Assertion
isOk v a = isOkLike v $ \x -> x @?= a

isFailLike :: (Show a, Exception e, Eq e) => Either SomeException a -> (e -> Assertion) -> Assertion
isFailLike (Right x) _ = fail $ "got ok " ++ show x
isFailLike (Left z) f =
  case fromException z of
    Nothing -> fail $ "incompatible: " ++ show z
    Just x -> f x

isFail :: (Show a, Exception e, Eq e) => Either SomeException a -> e -> Assertion
isFail v e = isFailLike v $ \x -> x @?= e

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
  runner task = expectRunner task

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
      taken = takeAllListT $ unfoldPlan simplePlan
      actual = runExpect taken config
  actual `isOk` expected

startTime :: UTCTime
startTime = UTCTime (toEnum 1) (fromIntegral 2)

runFS :: FakeFST (CatchT Identity) a -> FSDir -> Either SomeException (a, FSDir, [WatchEvent])
runFS fst dir = runIdentity $ runCatchT $ runFakeFST fst startTime dir

doFS :: FakeFST (CatchT Identity) a -> FSDir -> Either SomeException a
doFS fst dir = (\(x, _, _) -> x) <$> runFS fst dir

testFSRootExists :: TestTree
testFSRootExists = testCase "fs root exists" $ do
  let root = emptyFSDir
  doFS (doesDirectoryExistFS "/") root `isOk` True
  doFS (doesFileExistFS "/") root `isOk` False

testFSRead0 :: TestTree
testFSRead0 = testCase "fs read 0" $ do
  let fn = "/baz.txt"
      c = BLC.pack "hello"
      root = FSDir (M.singleton "baz.txt" (Right (FSFile c)))
  doFS (doesDirectoryExistFS fn) root `isOk` False
  doFS (doesFileExistFS fn) root `isOk` True
  doFS (readFileFS fn) root `isOk` c

testFSWrite0 :: TestTree
testFSWrite0 = testCase "fs write 0" $ do
  let fn = "/baz.txt"
      c = BLC.pack "hello"
      root = emptyFSDir
  doFS (doesFileExistFS fn) root `isOk` False
  runFS (writeFileFS fn c) root `isOkLike` \(_, fs, ev) -> do
    fs @?= FSDir (M.singleton "baz.txt" (Right (FSFile c)))
    ev @?= [WatchEvent AddedWatchEvent fn startTime]
  runFS (writeFileFS fn c >> writeFileFS fn c) root `isOkLike` \(_, fs, ev) -> do
    fs @?= FSDir (M.singleton "baz.txt" (Right (FSFile c)))
    ev @?= [WatchEvent AddedWatchEvent fn startTime, WatchEvent ModifiedWatchEvent fn startTime]
  doFS (writeFileFS fn c >> doesFileExistFS fn) root `isOk` True
  doFS (writeFileFS fn c >> readFileFS fn) root `isOk` c

testFSMkdir0 :: TestTree
testFSMkdir0 = testCase "fs mkdir 0" $ do
  let fn = "/boo"
      root = emptyFSDir
  doFS (doesDirectoryExistFS fn) root `isOk` False
  doFS (doesFileExistFS fn) root `isOk` False
  runFS (createDirectoryIfMissingFS False fn) root `isOkLike` \(_, fs, ev) -> do
    fs @?= FSDir (M.singleton "boo" (Left (FSDir M.empty)))
    ev @?= []  -- We emit watch events only for file modifications
  runFS (createDirectoryIfMissingFS True fn) root `isOkLike` \(_, fs, ev) -> do
    fs @?= FSDir (M.singleton "boo" (Left (FSDir M.empty)))
    ev @?= []
  runFS (createDirectoryIfMissingFS False fn >> createDirectoryIfMissingFS False fn) root `isOkLike` \(_, fs, ev) -> do
    fs @?= FSDir (M.singleton "boo" (Left (FSDir M.empty)))
    ev @?= []
  doFS (createDirectoryIfMissingFS False fn >> doesDirectoryExistFS fn) root `isOk` True
  doFS (createDirectoryIfMissingFS True fn >> doesDirectoryExistFS fn) root `isOk` True

testFSMkdirRec :: TestTree
testFSMkdirRec = testCase "fs mkdir recursive" $ do
  let fn = "/foo/bar/baz"
      root = emptyFSDir
  doFS (doesDirectoryExistFS fn) root `isOk` False
  doFS (createDirectoryIfMissingFS True fn >> doesDirectoryExistFS fn) root `isOk` True
  doFS (createDirectoryIfMissingFS False fn) root `isFail` MissingParent fn
  doFS (createDirectoryIfMissingFS True fn >> doesDirectoryExistFS fn) root `isOk` True
  runFS (createDirectoryIfMissingFS True fn) root `isOkLike` \(_, fs, _) -> do
    let c = FSDir (M.singleton "baz" (Left (FSDir M.empty)))
        b = FSDir (M.singleton "bar" (Left c))
        a = FSDir (M.singleton "foo" (Left b))
    fs @?= a

testFSWriteRec :: TestTree
testFSWriteRec = testCase "fs write recursive" $ do
  let dir = "/foo/bar/baz"
      fn = "/foo/bar/baz/quux.txt"
      c = BLC.pack "hello"
      root = emptyFSDir
  doFS (createDirectoryIfMissingFS True dir >> writeFileFS fn c >> readFileFS fn) root `isOk` c

testFS :: TestTree
testFS = testGroup "fs"
  [ testFSRootExists
  , testFSRead0
  , testFSWrite0
  , testFSMkdir0
  , testFSMkdirRec
  , testFSWriteRec
  ]

tests :: TestTree
tests = testGroup "tests"
  [ testSimple
  , testFS  
  ]

main :: IO ()
main = defaultMain tests
