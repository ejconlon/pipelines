{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Pipelines.Filesystem where

import           Control.Concurrent.Chan
import           Control.Monad            (unless)
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.RWS.Strict
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map.Strict          as M
import           Data.Time.Clock
import           Data.Typeable
import           List.Transformer
import           System.Directory
import           System.FilePath.Posix
import qualified System.FSNotify          as N

data NotAFile = NotAFile FilePath deriving (Show, Eq, Typeable)
instance Exception NotAFile

data NotADir = NotADir FilePath deriving (Show, Eq, Typeable)
instance Exception NotADir

data InvalidState = InvalidState String deriving (Show, Eq, Typeable)
instance Exception InvalidState

assertDirExists :: (MonadFS m, MonadThrow m) => FilePath -> m ()
assertDirExists path = do
  exists <- doesDirectoryExistFS path
  unless exists $ throwM $ NotADir path

assertFileExists :: (MonadFS m, MonadThrow m) => FilePath -> m ()
assertFileExists path = do
  exists <- doesFileExistFS path
  unless exists $ throwM $ NotAFile path

class MonadFS b where
  readFileFS :: FilePath -> b BL.ByteString
  writeFileFS :: FilePath -> BL.ByteString -> b ()
  doesFileExistFS :: FilePath -> b Bool
  doesDirectoryExistFS :: FilePath -> b Bool
  createDirectoryIfMissingFS :: Bool -> FilePath -> b ()
  renameFileFS :: FilePath -> FilePath -> b ()

touchFS :: MonadFS b => FilePath -> b ()
touchFS path = writeFileFS path BL.empty

instance MonadFS IO where
  readFileFS = BL.readFile
  writeFileFS = BL.writeFile
  doesFileExistFS = doesFileExist
  doesDirectoryExistFS = doesDirectoryExist
  createDirectoryIfMissingFS = createDirectoryIfMissing
  renameFileFS = renameFile

data FSFile = FSFile BL.ByteString deriving (Show, Eq)

data FSDir = FSDir (M.Map String (Either FSDir FSFile)) deriving (Show, Eq)

type FSEntity = Either FSDir FSFile

isDir :: FSEntity -> Bool
isDir (Left (FSDir _)) = True
isDir _ = False

isFile :: FSEntity -> Bool
isFile (Right (FSFile _)) = True
isFile _ = False

emptyFSDir :: FSDir
emptyFSDir = FSDir M.empty

newtype FakeFST b a = FakeFST
  { unFakeFST :: RWST UTCTime [WatchEvent] FSDir b a
  } deriving (Functor, Applicative, Monad, MonadReader UTCTime, MonadState FSDir, MonadThrow)

-- | Monad boilerplate
instance MonadTrans FakeFST where
  lift = FakeFST . lift

-- | Monad boilerplate
instance Monad b => MonadBase b (FakeFST b) where
  liftBase = lift

getEntityParts :: [String] -> FSEntity -> Maybe FSEntity
getEntityParts [] e = Just e
getEntityParts (_:_) (Right _) = Nothing
getEntityParts (p:ps) (Left (FSDir m)) = M.lookup p m >>= getEntityParts ps

writeFileParts :: MonadThrow m => FilePath -> [String] -> BL.ByteString -> FSDir -> m FSDir
writeFileParts orig [] _ _ = throwM $ NotAFile orig
writeFileParts orig [p] contents (FSDir m) =
  case M.lookup p m of
    Just (Left (FSDir _)) -> throwM $ NotAFile orig
    _ -> do
      let child = FSFile contents
      return $ FSDir $ M.insert p (Right child) m
writeFileParts orig (p:ps) contents (FSDir m) =
  case M.lookup p m of
    Just (Left d@(FSDir n)) -> do
      child <- writeFileParts orig ps contents d
      return $ FSDir $ M.insert p (Left child) n
    _ -> throwM $ NotAFile orig

getEntity :: Monad b => FilePath -> FakeFST b (Maybe FSEntity)
getEntity path = do
  root <- get
  let parts = drop 1 $ splitPath path
  return $ getEntityParts parts (Left root)

readFileFST :: MonadThrow b => FilePath -> FakeFST b BL.ByteString
readFileFST path = do
  entity <- getEntity path
  case entity of
    Just (Right (FSFile b)) -> return b
    _ -> throwM $ NotAFile path

writeFileFST :: MonadThrow b => FilePath -> BL.ByteString -> FakeFST b ()
writeFileFST path contents = do
  root <- get
  let parts = drop 1 $ splitPath path
  newRoot <- liftBase $ writeFileParts path parts contents root
  put newRoot
        
doesFileExistFST :: Monad b => FilePath -> FakeFST b Bool
doesFileExistFST path = do
  entity <- getEntity path
  return $ case entity of
             Just (Right (FSFile _)) -> True
             _ -> False

doesDirectoryExistFST :: Monad b => FilePath -> FakeFST b Bool
doesDirectoryExistFST path = do
  entity <- getEntity path
  return $ case entity of
             Just (Left (FSDir _)) -> True
             _ -> False

createDirectoryIfMissingFST :: Bool -> FilePath -> FakeFST b ()
createDirectoryIfMissingFST = undefined

renameFileFST :: FilePath -> FilePath -> FakeFST b ()
renameFileFST = undefined

instance MonadThrow b => MonadFS (FakeFST b) where
  readFileFS = readFileFST
  writeFileFS = writeFileFST
  doesFileExistFS = doesFileExistFST
  doesDirectoryExistFS = doesDirectoryExistFST
  createDirectoryIfMissingFS = createDirectoryIfMissingFST
  renameFileFS = renameFileFST

runFakeFST :: FakeFST b a -> UTCTime -> FSDir -> b (a, FSDir, [WatchEvent]) 
runFakeFST (FakeFST x) = runRWST x

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
