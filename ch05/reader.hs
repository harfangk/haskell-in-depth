{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.Reader

newtype Config = Config
  { verbose :: Bool
  }

type ConfigM = Reader Config

work :: ConfigM ()
work = do
  doSomething

doSomething :: ConfigM ()
doSomething = do
  doSomethingSpecial

doSomethingSpecial :: ConfigM ()
doSomethingSpecial = do
  Config {verbose} <- ask
  when verbose beVerbose

beVerbose :: ConfigM ()
beVerbose = pure ()

silent :: Config -> Config
silent config = config {verbose = False}

doSomethingSpecialSilently :: ConfigM ()
doSomethingSpecialSilently = local silent doSomethingSpecial

getConfiguration :: IO Config
getConfiguration = pure Config {verbose = True}

main :: IO ()
main = do
  config <- getConfiguration
  let result = runReader work config
  print result
