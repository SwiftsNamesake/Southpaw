module Paths_Southpaw (
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

bindir     = "C:\\Program Files (x86)\\Haskell\\bin"
libdir     = "C:\\Program Files (x86)\\Haskell\\i386-windows-ghc-7.6.3\\Southpaw-0.1.0.0"
datadir    = "C:\\Program Files (x86)\\Haskell\\i386-windows-ghc-7.6.3\\Southpaw-0.1.0.0"
libexecdir = "C:\\Program Files (x86)\\Haskell\\Southpaw-0.1.0.0"
sysconfdir = "C:\\Program Files (x86)\\Haskell\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Southpaw_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Southpaw_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Southpaw_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Southpaw_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Southpaw_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
