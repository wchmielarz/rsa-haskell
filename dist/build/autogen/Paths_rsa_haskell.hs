module Paths_rsa_haskell (
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

bindir     = "/home/gjasinski/.cabal/bin"
libdir     = "/home/gjasinski/.cabal/lib/x86_64-linux-ghc-7.10.3/rsa-haskell-0.1.0.0-JDhANR6OGSc0xN814igI0y"
datadir    = "/home/gjasinski/.cabal/share/x86_64-linux-ghc-7.10.3/rsa-haskell-0.1.0.0"
libexecdir = "/home/gjasinski/.cabal/libexec"
sysconfdir = "/home/gjasinski/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "rsa_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "rsa_haskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "rsa_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rsa_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "rsa_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
