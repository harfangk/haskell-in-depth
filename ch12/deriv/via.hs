{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

import Data.Monoid

newtype Age = Age Int
  deriving newtype (Eq, Ord)

newtype Age' = Age' Int
  deriving (Eq, Ord) via Int

newtype MAge = MAge (Maybe Int)

-- deriving (Semigroup, Monoid) via (Alt Maybe Int)

deriving via (Alt Maybe Int) instance Semigroup MAge

deriving via (Alt Maybe Int) instance Monoid MAge

main :: IO ()
main = undefined
