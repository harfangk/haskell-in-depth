{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pointers where

import Data.Proxy
import GHC.TypeLits

newtype Pointer (align :: Nat) = Pointer Integer

zeroPtr :: Pointer n
zeroPtr = Pointer 0

inc :: Pointer align -> Pointer align
inc (Pointer p) = Pointer (p + 1)

ptrValue :: forall align. KnownNat align => Pointer align -> Integer
ptrValue (Pointer p) = p * natVal (Proxy :: Proxy align)

maybePtr :: forall align. KnownNat align => Integer -> Maybe (Pointer align)
maybePtr p
  | remainder == 0 = Just (Pointer quotient)
  | otherwise = Nothing
 where
  (quotient, remainder) = divMod p (natVal (Proxy :: Proxy align))
