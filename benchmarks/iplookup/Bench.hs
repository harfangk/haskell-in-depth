module Main where

-- import BenchBuildIP
import BenchBuildIPGroups
import BenchLookupIP
import BenchParseIP
import BenchRanges
import Criterion.Main

main :: IO ()
main = defaultMain benchmarks
  where
    benchmarks =
      bench_buildIP -- <> bench_buildIP_list
        <> bench_parseIP
        <> bench_ranges
        <> bench_lookupIP
