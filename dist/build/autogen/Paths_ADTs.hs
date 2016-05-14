module Paths_ADTs (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jem/.cabal/bin"
libdir     = "/Users/jem/.cabal/lib/x86_64-osx-ghc-7.10.2/ADTs-0.1.0.0-1ivJ93tEz4T26zTpYtArmv"
datadir    = "/Users/jem/.cabal/share/x86_64-osx-ghc-7.10.2/ADTs-0.1.0.0"
libexecdir = "/Users/jem/.cabal/libexec"
sysconfdir = "/Users/jem/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ADTs_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ADTs_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ADTs_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ADTs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ADTs_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
