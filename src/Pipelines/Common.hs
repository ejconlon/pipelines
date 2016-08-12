module Pipelines.Common where

import Control.Concurrent.Chan
import List.Transformer
import Pipelines.Types

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

-- | Takes all elements from the ListT (blocks and returns all at once, if at all)
takeAllListT :: Monad b => ListT b a -> b [a]
takeAllListT (ListT mStep) = do
  step <- mStep
  case step of
    Nil -> return []
    Cons a rest -> (a:) <$> takeAllListT rest

readChanToList :: Chan a -> ListT IO a
readChanToList c = ListT $ do
  putStrLn "reading chan"
  value <- readChan c
  putStrLn "got value"
  return $ Cons value $ readChanToList c

class Monad b => MonadCommand b where
  command :: Action -> b Result

-- | The thing that actually runs tasks.
-- Given a plan name and a stack of task results,
-- runs a plan and returns a result in context.
-- Newtype this to control how tasks are run:
-- A real implementation might work in IO over the filesystem.
-- An implementation for tests might work over State and yield fake history.
class Monad b => MonadRunner b where
  runner :: Task -> b Result
