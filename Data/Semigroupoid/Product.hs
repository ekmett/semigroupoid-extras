{-# LANGUAGE GADTs #-}
module Data.Semigroupoid.Product where

import Data.Semigroupoid

data Product j k a b where
  Pair :: j a b -> k a' b' -> Product j k (a,a') (b,b')

instance (Semigroupoid j, Semigroupoid k) => Semigroupoid (Product j k) where
  Pair w x `o` Pair y z = Pair (w `o` y) (x `o` z)
