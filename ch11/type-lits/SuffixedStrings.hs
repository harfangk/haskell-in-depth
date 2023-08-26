{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SuffixedStrings where

import Data.Proxy
import GHC.TypeLits

data SuffixedString (suffix :: Symbol) = SS String

suffixed :: String -> SuffixedString suffix
suffixed s = SS s

asString :: forall suffix. KnownSymbol suffix => SuffixedString suffix -> String
asString (SS str) = str ++ "@" ++ symbolVal (Proxy :: Proxy suffix)
