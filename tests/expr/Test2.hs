{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import qualified Data.Text as T
import Expr
import ShuntingYard hiding (main)
import System.Exit
import TextShow

strs :: [String]
strs =
  [ "42",
    "12 + 13",
    "(2+3*3)*5",
    "1+(1+2)*(2+2*(1+2))+1+3*2",
    "13+2+12+2+1+2+13+2",
    "1*2*132*22*1*22*0*2",
    "10*(1+2)*2*(2+1*2)+1+3+2"
  ]

exprs :: [Expr Integer]
exprs = map convertToExpr strs

exprs' :: [Expr Integer]
exprs' = map (convertToExpr . T.unpack . showt) exprs

correct :: Bool
correct = and $ zipWith (\e1 e2 -> myeval e1 == myeval e2) exprs exprs'

main :: IO ()
main = unless correct exitFailure
