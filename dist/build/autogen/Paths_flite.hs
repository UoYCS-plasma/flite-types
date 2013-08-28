module Paths_flite (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,4,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/marcoper/.cabal/bin"
libdir     = "/usr/marcoper/.cabal/lib/flite-0.4.0/ghc-7.0.3"
datadir    = "/usr/marcoper/.cabal/share/flite-0.4.0"
libexecdir = "/usr/marcoper/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "flite_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "flite_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "flite_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "flite_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
