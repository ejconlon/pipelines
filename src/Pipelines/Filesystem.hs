module Pipelines.Filesystem
  ( assertDirExists
  , assertFileExists  
  , MonadFS(..)
  , MonadWatch(..)
  , PathDoesNotExist(..)  
  , Watch(..)
  , WatchEventType(..)
  , WatchEvent(..)
  ) where

import           Control.Concurrent.Chan
import           Control.Monad           (unless)
import           Control.Monad.Catch
import qualified Data.ByteString.Lazy    as BL
import           Data.Time.Clock
import           Data.Typeable
import           List.Transformer
import           System.Directory
import qualified System.FSNotify         as N

data PathDoesNotExist = PathDoesNotExist FilePath deriving (Show, Eq, Typeable)
instance Exception PathDoesNotExist

assertDirExists :: (MonadFS m, MonadThrow m) => FilePath -> m ()
assertDirExists path = do
  exists <- doesDirectoryExistFS path
  unless exists $ throwM $ PathDoesNotExist path

assertFileExists :: (MonadFS m, MonadThrow m) => FilePath -> m ()
assertFileExists path = do
  exists <- doesFileExistFS path
  unless exists $ throwM $ PathDoesNotExist path

class MonadFS b where
  readFileFS :: FilePath -> b BL.ByteString
  writeFileFS :: FilePath -> BL.ByteString -> b ()
  doesFileExistFS :: FilePath -> b Bool
  doesDirectoryExistFS :: FilePath -> b Bool
  createDirectoryIfMissingFS :: Bool -> FilePath -> b ()
  renameFileFS :: FilePath -> FilePath -> b ()

instance MonadFS IO where
  readFileFS = BL.readFile
  writeFileFS = BL.writeFile
  doesFileExistFS = doesFileExist
  doesDirectoryExistFS = doesDirectoryExist
  createDirectoryIfMissingFS = createDirectoryIfMissing
  renameFileFS = renameFile

data WatchEventType = AddedWatchEvent | ModifiedWatchEvent | RemovedWatchEvent deriving (Show, Eq)

data WatchEvent = WatchEvent
  { _watchEventType :: WatchEventType
  , _watchEventPath :: FilePath
  , _watchEventTime :: UTCTime
  } deriving (Show, Eq)

data Watch b c = Watch
  { _watchEvents :: ListT b (WatchEvent, c)
  , _watchStop   :: b ()
  }

instance Monad b => Monoid (Watch b c) where
  mempty = Watch (ListT (return Nil)) (return ())
  mappend (Watch e1 s1) (Watch e2 s2) = Watch (e1 <|> e2) (s1 >> s2)

eventToWatchEvent :: N.Event -> WatchEvent
eventToWatchEvent (N.Added p t) = WatchEvent AddedWatchEvent p t
eventToWatchEvent (N.Modified p t) = WatchEvent ModifiedWatchEvent p t
eventToWatchEvent (N.Removed p t) = WatchEvent RemovedWatchEvent p t

watchEventToEvent :: WatchEvent -> N.Event
watchEventToEvent (WatchEvent ty p t) =
  case ty of
    AddedWatchEvent -> N.Added p t
    ModifiedWatchEvent -> N.Modified p t
    RemovedWatchEvent -> N.Removed p t

readChanToList :: Chan a -> ListT IO a
readChanToList c = ListT $ do
  value <- readChan c
  return $ Cons value $ readChanToList c

class MonadWatch b where
  watchDir :: FilePath -> (WatchEvent -> Bool) -> c -> b (Watch b c)

instance MonadWatch IO where
  watchDir path pred ctx = N.withManager $ \manager -> do
    chan <- newChan
    stop <- N.watchDirChan manager path (pred . eventToWatchEvent) chan
    let list = (\e -> (eventToWatchEvent e, ctx)) <$> readChanToList chan
    return $ Watch list stop
