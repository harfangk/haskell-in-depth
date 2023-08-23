import Criterion.Main
import qualified IsPrime as IP
import qualified IsPrimeUnfolded as IPU

primeNumber :: Integer
primeNumber = 16183

isPrime :: Integer -> Bool
isPrime n = all notDividedBy [2 .. n `div` 2]
  where
    notDividedBy m = n `mod` m /= 0

main :: IO ()
main =
  defaultMain
    [ bench "isPrime (declarative)" $ whnf IP.isPrime primeNumber,
      bench "isPrime (unfolded)" $ whnf IPU.isPrime primeNumber,
      bench "isPrime (rewritten)" $ whnf isPrime primeNumber
    ]
