module Paths_dejong (
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

bindir     = "/Users/mikifossati/yc/haskell/dejong/.stack-work/install/x86_64-osx/lts-5.17/7.10.3/bin"
libdir     = "/Users/mikifossati/yc/haskell/dejong/.stack-work/install/x86_64-osx/lts-5.17/7.10.3/lib/x86_64-osx-ghc-7.10.3/dejong-0.1.0.0-ArrETO7fLCKDnVObK0NxeK"
datadir    = "/Users/mikifossati/yc/haskell/dejong/.stack-work/install/x86_64-osx/lts-5.17/7.10.3/share/x86_64-osx-ghc-7.10.3/dejong-0.1.0.0"
libexecdir = "/Users/mikifossati/yc/haskell/dejong/.stack-work/install/x86_64-osx/lts-5.17/7.10.3/libexec"
sysconfdir = "/Users/mikifossati/yc/haskell/dejong/.stack-work/install/x86_64-osx/lts-5.17/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "dejong_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "dejong_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "dejong_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "dejong_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "dejong_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
