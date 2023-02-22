{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_sum_of_multiples (
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
version = Version [1,5,0,10] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Ethio\\Exercism\\haskell\\haskell-progress\\eyob\\sum-of-multiples\\.stack-work\\install\\59205620\\bin"
libdir     = "C:\\Users\\Ethio\\Exercism\\haskell\\haskell-progress\\eyob\\sum-of-multiples\\.stack-work\\install\\59205620\\lib\\x86_64-windows-ghc-9.0.2\\sum-of-multiples-1.5.0.10-J6txB5gFlbv6v75GBjm3tl-test"
dynlibdir  = "C:\\Users\\Ethio\\Exercism\\haskell\\haskell-progress\\eyob\\sum-of-multiples\\.stack-work\\install\\59205620\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "C:\\Users\\Ethio\\Exercism\\haskell\\haskell-progress\\eyob\\sum-of-multiples\\.stack-work\\install\\59205620\\share\\x86_64-windows-ghc-9.0.2\\sum-of-multiples-1.5.0.10"
libexecdir = "C:\\Users\\Ethio\\Exercism\\haskell\\haskell-progress\\eyob\\sum-of-multiples\\.stack-work\\install\\59205620\\libexec\\x86_64-windows-ghc-9.0.2\\sum-of-multiples-1.5.0.10"
sysconfdir = "C:\\Users\\Ethio\\Exercism\\haskell\\haskell-progress\\eyob\\sum-of-multiples\\.stack-work\\install\\59205620\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sum_of_multiples_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sum_of_multiples_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "sum_of_multiples_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "sum_of_multiples_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sum_of_multiples_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sum_of_multiples_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
