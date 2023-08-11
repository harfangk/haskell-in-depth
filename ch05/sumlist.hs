import Control.Monad.State
import Data.Foldable

addItem :: Integer -> State Integer ()
addItem n = do
  s <- get
  put (s + n)

addItem' :: Integer -> State Integer ()
addItem' n = modify' (+ n)

sumList :: [Integer] -> State Integer ()
sumList xs = traverse_ addItem xs

main :: IO ()
main = print ""
