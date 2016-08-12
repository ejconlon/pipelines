module Pipelines.Common where

import List.Transformer

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
