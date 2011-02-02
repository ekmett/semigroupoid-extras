{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Data.Semigroupoid.Ob where

import Data.Semigroupoid
import Data.Semigroupoid.Product 
import Data.Semigroupoid.Coproduct
import Control.Comonad
import Data.Functor.Bind
import Control.Arrow

class Semigroupoid k => Ob k a where
  semiid :: k a a

instance (Ob l a, Ob r b) => Ob (Product l r) (a,b) where
  semiid = Pair semiid semiid

instance (Ob l a, Semigroupoid r)  => Ob (Coproduct l r) (L a) where
  semiid = L semiid

instance (Semigroupoid l, Ob r a) => Ob (Coproduct l r) (R a) where
  semiid = R semiid

instance (Bind m, Monad m, Ob (->) a) => Ob (Kleisli m) a where
  semiid = Kleisli return

instance (Comonad w, Ob (->) a) => Ob (Cokleisli w) a where
  semiid = Cokleisli extract
