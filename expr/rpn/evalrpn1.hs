import EvalRPN
import System.Environment

evalPrintExpr :: String -> IO ()
evalPrintExpr str = do
  let r = evalRPN str
  putStrLn $ str ++ " = " ++ show r

main :: IO ()
main = getArgs >>= mapM_ evalPrintExpr
