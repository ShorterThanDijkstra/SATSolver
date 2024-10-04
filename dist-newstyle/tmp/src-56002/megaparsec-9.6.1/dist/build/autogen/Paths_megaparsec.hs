{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_megaparsec (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [9,6,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/maerd_zinbiel/.cabal/store/ghc-9.4.8/megaparsec-9.6.1-7833a24332826493035e65a01ee3cf7b5a6cc8a95024cb2185b35b3ca9ba79c8/bin"
libdir     = "/home/maerd_zinbiel/.cabal/store/ghc-9.4.8/megaparsec-9.6.1-7833a24332826493035e65a01ee3cf7b5a6cc8a95024cb2185b35b3ca9ba79c8/lib"
dynlibdir  = "/home/maerd_zinbiel/.cabal/store/ghc-9.4.8/megaparsec-9.6.1-7833a24332826493035e65a01ee3cf7b5a6cc8a95024cb2185b35b3ca9ba79c8/lib"
datadir    = "/home/maerd_zinbiel/.cabal/store/ghc-9.4.8/megaparsec-9.6.1-7833a24332826493035e65a01ee3cf7b5a6cc8a95024cb2185b35b3ca9ba79c8/share"
libexecdir = "/home/maerd_zinbiel/.cabal/store/ghc-9.4.8/megaparsec-9.6.1-7833a24332826493035e65a01ee3cf7b5a6cc8a95024cb2185b35b3ca9ba79c8/libexec"
sysconfdir = "/home/maerd_zinbiel/.cabal/store/ghc-9.4.8/megaparsec-9.6.1-7833a24332826493035e65a01ee3cf7b5a6cc8a95024cb2185b35b3ca9ba79c8/etc"

getBinDir     = catchIO (getEnv "megaparsec_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "megaparsec_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "megaparsec_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "megaparsec_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "megaparsec_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "megaparsec_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
