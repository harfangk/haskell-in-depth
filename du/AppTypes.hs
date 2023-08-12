module AppTypes where

import System.PosixCompat.Files

data AppConfig = AppConfig
  { basePath :: FilePath,
    maxDepth :: Int,
    extension :: Maybe String,
    followSymLinks :: Bool
  }

data AppEnv = AppEnv
  { cfg :: AppConfig,
    path :: FilePath,
    depth :: Int,
    fileStatus :: FilePath -> IO FileStatus
  }

initialEnv :: AppConfig -> AppEnv
initialEnv config@AppConfig {..} =
  AppEnv
    { cfg = config,
      path = basePath,
      depth = maxDepth,
      fileStatus =
        if followSymLinks
          then getFileStatus
          else getSymbolicLinkStatus
    }
