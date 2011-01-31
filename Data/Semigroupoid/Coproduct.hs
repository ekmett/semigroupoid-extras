{-# LANGUAGE GADTs, EmptyDataDecls #-}
module Data.Semigroupoid.Coproduct 
  ( L, R, Coproduct(..) ) where

import Data.Semigroupoid

data L a
data R a

data Coproduct j k a b where
  L :: j a b -> Coproduct j k (L a) (L b)
  R :: k a b -> Coproduct j k (R a) (R b)

instance (Semigroupoid j, Semigroupoid k) => Semigroupoid (Coproduct j k) where
  L f `o` L g = L (f `o` g)
  R f `o` R g = R (f `o` g)
  _ `o` _ = error "GADT fail"
