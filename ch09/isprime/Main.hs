module Main where

import IsPrime
import System.Environment
import System.TimeIt

main :: IO ()
main = getArgs >>= timeIt . print . isPrime . read . head
