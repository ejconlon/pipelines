{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
