{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppRWSTST where

import AppTypes
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

newtype MyApp logEntry state a = MyApp
  {runApp :: ReaderT AppEnv (WriterT [logEntry] (StateT state IO)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader AppEnv,
      MonadWriter [logEntry],
      MonadState state
    )

runMyApp :: MyApp logEntry state a -> AppConfig -> state -> IO (a, [logEntry])
runMyApp app config =
  evalStateT (runWriterT (runReaderT (runApp app) (initialEnv config)))
