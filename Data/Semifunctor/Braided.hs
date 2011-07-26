{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, GADTs #-}
module Data.Semifunctor.Braided 
  ( Braided(..)
  , kleisliBraid
  , cokleisliBraid
  , Symmetric
  , swap
  ) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Comonad
import Data.Functor.Bind
import Data.Semifunctor
import Data.Semifunctor.Associative
-- import Data.Semigroupoid.Dual

class Associative k p => Braided k p where
  braid :: k (p(a,b)) (p(b,a))

-- instance Braided k p => Braided (Dual k) p where braid = Dual braid

instance Braided (->) (Bi Either) where
  braid (Bi (Left a)) = Bi (Right a)
  braid (Bi (Right a)) = Bi (Left a)

instance Braided (->) (Bi (,)) where
  braid (Bi (a,b)) = Bi (b,a)

kleisliBraid :: (Monad m, Semifunctor p (Product (Kleisli m) (Kleisli m)) (Kleisli m), Braided (->) p) => Kleisli m (p(a,b)) (p(b,a))
kleisliBraid = Kleisli (return . braid)

instance (Bind m, Monad m) => Braided (Kleisli m) (Bi Either) where
  braid = kleisliBraid

instance (Bind m, Monad m) => Braided (Kleisli m) (Bi (,)) where
  braid = kleisliBraid

cokleisliBraid :: (Comonad w, Semifunctor p (Product (Cokleisli w) (Cokleisli w)) (Cokleisli w), Braided (->) p) => Cokleisli w (p(a,b)) (p(b,a))
cokleisliBraid = Cokleisli (braid . extract)

instance Comonad w => Braided (Cokleisli w) (Bi (,)) where
  braid = cokleisliBraid

-- instance Comonad w => Braided (Cokleisli w) (Bi Either) where braid = cokleisliBraid

class Braided k p => Symmetric k p
instance Symmetric (->) (Bi Either) 
instance Symmetric (->) (Bi (,))
instance (Bind m, Monad m) => Symmetric (Kleisli m) (Bi Either)
instance (Bind m, Monad m) => Symmetric (Kleisli m) (Bi (,))
instance Comonad w => Symmetric (Cokleisli w) (Bi (,))
-- instance Comonad w => Symmetric (Cokleisli w) (Bi Either)

swap :: Symmetric k p => k (p(a,b)) (p(b,a))
swap = braid
