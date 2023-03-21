{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_TP3 (
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

bindir     = "/mnt/c/Users/usuario/Desktop/4to/ALP/tpsALP/An\225lisis de Lenguajes de Programaci\243n/tp3/TP3/.stack-work/install/x86_64-linux-tinfo6/fb1165d7b11e856a1751b3b1e9fb741d9e0ce10c07ffb4af5c93a4f646dbb627/8.8.3/bin"
libdir     = "/mnt/c/Users/usuario/Desktop/4to/ALP/tpsALP/An\225lisis de Lenguajes de Programaci\243n/tp3/TP3/.stack-work/install/x86_64-linux-tinfo6/fb1165d7b11e856a1751b3b1e9fb741d9e0ce10c07ffb4af5c93a4f646dbb627/8.8.3/lib/x86_64-linux-ghc-8.8.3/TP3-0.1.0.0-61JFx0PafwMAHx7Vi0mYMR-TP3-exe"
dynlibdir  = "/mnt/c/Users/usuario/Desktop/4to/ALP/tpsALP/An\225lisis de Lenguajes de Programaci\243n/tp3/TP3/.stack-work/install/x86_64-linux-tinfo6/fb1165d7b11e856a1751b3b1e9fb741d9e0ce10c07ffb4af5c93a4f646dbb627/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/mnt/c/Users/usuario/Desktop/4to/ALP/tpsALP/An\225lisis de Lenguajes de Programaci\243n/tp3/TP3/.stack-work/install/x86_64-linux-tinfo6/fb1165d7b11e856a1751b3b1e9fb741d9e0ce10c07ffb4af5c93a4f646dbb627/8.8.3/share/x86_64-linux-ghc-8.8.3/TP3-0.1.0.0"
libexecdir = "/mnt/c/Users/usuario/Desktop/4to/ALP/tpsALP/An\225lisis de Lenguajes de Programaci\243n/tp3/TP3/.stack-work/install/x86_64-linux-tinfo6/fb1165d7b11e856a1751b3b1e9fb741d9e0ce10c07ffb4af5c93a4f646dbb627/8.8.3/libexec/x86_64-linux-ghc-8.8.3/TP3-0.1.0.0"
sysconfdir = "/mnt/c/Users/usuario/Desktop/4to/ALP/tpsALP/An\225lisis de Lenguajes de Programaci\243n/tp3/TP3/.stack-work/install/x86_64-linux-tinfo6/fb1165d7b11e856a1751b3b1e9fb741d9e0ce10c07ffb4af5c93a4f646dbb627/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TP3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TP3_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "TP3_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "TP3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TP3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TP3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
