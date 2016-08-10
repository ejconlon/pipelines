{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
Design:

config:
workdir
plan json paths

validation:
workdir exists
plan json paths exist and parse correctly

preparation:
create plan directories
- create input subdir
- create task subdir
- create state subdir
- create archive subdir

execution:
watch input dir
invoke plans on new inputs
update state dir with state changes
if finished, move input to archive
can cancel by moving input to archive manually

remote access:
run web interface that can
- list running plans
- list archived plans
- create new inputs
- stop running plans
- show plan state
- clone and restart archived plans
-}
module Local where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Pipelines

data LocalConfig = LocalConfig { }

type MonadLocal m = (Monad m, MonadReader LocalConfig m, MonadIO m)

newtype LocalRunner a = LocalRunner
  { unLocalRunner :: ReaderT LocalConfig IO a }
  deriving (Functor, Applicative, Monad, MonadReader LocalConfig, MonadIO)

localRunner :: MonadLocal m => Plan -> [History] -> Task -> m History
localRunner = undefined

instance MonadRunner LocalRunner where
  runner = localRunner

-- TODO parse config and Plan
runLocal :: IO ()
runLocal = return ()
