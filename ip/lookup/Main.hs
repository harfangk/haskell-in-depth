module Main where

import Control.Monad.Catch
import IPTypes
import LookupIP
import Options.Applicative as Opt
import ParseIP
import System.Exit

data Params = Params FilePath String

mkParams :: Opt.Parser Params
mkParams =
  Params
    <$> argument str (metavar "FILE" <> help "IP range database")
    <*> argument str (metavar "ip" <> help "IP address to check")

run :: Params -> IO ()
run (Params fp ipstr) = do
  iprs <- parseIPRanges <$> readFile fp
  case (iprs, parseIP ipstr) of
    (_, Nothing) -> throwM $ InvalidIP ipstr
    (Left pe, _) -> throwM $ LoadIPRangesError pe
    (Right iprdb, Just ip) -> putStrLn $ reportIPs iprdb [ip]

main :: IO ()
main = (execParser opts >>= run) `catches` [Handler parserExit]
  where
    opts =
      info
        (mkParams <**> helper)
        ( fullDesc
            <> progDesc
              ( "Answers YES/NO depending on whether "
                  ++ "an IP address belongs to the IP range database"
              )
        )
    parserExit :: ExitCode -> IO ()
    parserExit _ = pure ()
