module Paths_type_sym_th (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1,0,4], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/hi5networks/.cabal/bin"
libdir     = "/Users/hi5networks/.cabal/lib/type-sym-th-0.1.0.4/ghc-7.2.2"
datadir    = "/Users/hi5networks/.cabal/share/type-sym-th-0.1.0.4"
libexecdir = "/Users/hi5networks/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "type_sym_th_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "type_sym_th_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "type_sym_th_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "type_sym_th_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
