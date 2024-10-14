{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ropeak_snake (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ronan/.cabal/bin"
libdir     = "/home/ronan/.cabal/lib/x86_64-linux-ghc-8.8.4/ropeak-snake-0.1.0.0-inplace"
dynlibdir  = "/home/ronan/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/ronan/.cabal/share/x86_64-linux-ghc-8.8.4/ropeak-snake-0.1.0.0"
libexecdir = "/home/ronan/.cabal/libexec/x86_64-linux-ghc-8.8.4/ropeak-snake-0.1.0.0"
sysconfdir = "/home/ronan/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ropeak_snake_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ropeak_snake_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ropeak_snake_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ropeak_snake_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ropeak_snake_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ropeak_snake_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
