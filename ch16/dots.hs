import Control.Concurrent
import Control.Monad (forever)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

oneSec :: Int
oneSec = 1000000

doSomethingUseful :: IO ()
doSomethingUseful = do
  threadDelay $ 5 * oneSec
  putStrLn "All done"

printDots :: Int -> IO ()
printDots msec = forever $ do
  putStrLn "."
  threadDelay msec

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Doing something useful"
  dotsPrinter <- forkIO (printDots oneSec)
  doSomethingUseful
  killThread dotsPrinter
  putStrLn "Exiting..."
