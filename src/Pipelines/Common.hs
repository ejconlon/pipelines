module Pipelines.Common where

import Control.Concurrent.Chan
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

-- | Supposed to be a fair >>=
class Monad b => MonadDiagonal b where
  diagonal :: ListT b x -> (x -> ListT b y) -> ListT b y
  -- interleave :: ListT b x -> ListT b x -> ListT b x
  -- interleaveAll :: [ListT b x] -> ListT b x

instance MonadDiagonal IO where
  diagonal l f = l >>= f  -- TODO there must be a fair version of this

data Watch b c = Watch
  { _watchEvents :: ListT b c
  , _watchStop   :: b ()
  }

-- TODO is <|> fair?
instance Monad b => Monoid (Watch b c) where
  mempty = Watch (ListT (return Nil)) (return ())
  mappend (Watch e1 s1) (Watch e2 s2) = Watch (e1 <|> e2) (s1 >> s2)

readChanToList :: Chan a -> ListT IO a
readChanToList c = ListT $ do
  value <- readChan c
  return $ Cons value $ readChanToList c
