module BenchLookupIP where

import Criterion.Main
import Data
import qualified FastLookup as FL
import LookupIP
import NFUtils ()

bench_lookupIP :: [Benchmark]
bench_lookupIP =
  [ env iprdb $ \iprdb' ->
      bgroup
        "lookupIP"
        [ bgroup "single" $
            map (\(textip, ip) -> bench textip $ whnf (lookupIP iprdb') ip) ips,
          bench "several" $ nf (map (lookupIP iprdb')) $ map snd ips
        ],
    env iprdb $ \iprdb' ->
      let fiprdb = FL.fromIPRangeDB iprdb'
       in bgroup
            "lookupIP (fast)"
            [ bgroup "single" $
                map (\(textip, ip) -> bench textip $ whnf (FL.lookupIP fiprdb) ip) ips,
              bench "several" $ nf (map (FL.lookupIP fiprdb)) $ map snd ips
            ]
  ]
