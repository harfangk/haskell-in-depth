import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import System.IO (BufferMode (..), hSetBuffering, stdout)

data BinTree
  = Node Int BinTree BinTree
  | EmptyTree

tree :: BinTree
tree = Node 2 (Node 3 EmptyTree EmptyTree) (Node 1 EmptyTree EmptyTree)

treeMax :: BinTree -> Int
treeMax EmptyTree = 0
treeMax (Node n left right) = max n $ max (treeMax left) (treeMax right)

oneSec :: Int
oneSec = 1000000

work :: Int -> IO ()
work sec = do
  threadDelay $ sec * oneSec
  putStrLn $ "Work is completed for " ++ show sec ++ " sec"

spawnTree :: BinTree -> IO ()
spawnTree EmptyTree = pure ()
spawnTree (Node sec left right) = do
  _ <- async (spawnTree left)
  _ <- async (spawnTree right)
  work sec

spawnTreeCancel :: BinTree -> IO ()
spawnTreeCancel EmptyTree = pure ()
spawnTreeCancel (Node sec left right) =
  withAsync (spawnTree left) $ \_ ->
    withAsync (spawnTree right) $ \_ ->
      work sec

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn $ "No children cancellation"
  spawnTree tree
  threadDelay $ (1 + treeMax tree) * oneSec
  putStrLn $ "\nWith children cancellation"
  spawnTreeCancel tree
  threadDelay $ (1 + treeMax tree) * oneSec
  putStrLn "Exiting..."
