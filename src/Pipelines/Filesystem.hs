module Pipelines.Filesystem where

import qualified Data.ByteString.Lazy as BL
import System.Directory

class MonadFS b where
  readFileFS :: FilePath -> b BL.ByteString
  writeFileFS :: FilePath -> BL.ByteString -> b ()
  doesFileExistFS :: FilePath -> b Bool
  doesDirectoryExistFS :: FilePath -> b Bool

instance MonadFS IO where
  readFileFS = BL.readFile
  writeFileFS = BL.writeFile
  doesFileExistFS = doesFileExist
  doesDirectoryExistFS = doesDirectoryExist
