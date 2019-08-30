{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Generics.Traversable.Generic where

import Data.Generics.Traversable
import GHC.Generics

class GTraversable' c (f :: * -> *) where
  gtraverse' :: Applicative g => (forall d. c d => d -> g d) -> (forall p. f p -> g (f p))

instance GTraversable' c U1 where
  gtraverse' _f U1 = pure U1

instance GTraversable' c V1 where
  gtraverse' _f = error "Impossible"

instance GTraversable' c Par1 where
  gtraverse' _f par1 = pure par1

instance (forall p. GTraversable c (f p)) => GTraversable' c (Rec1 f) where
  gtraverse' f (Rec1 recur) = Rec1 <$> gtraverse @c f recur

instance (c con) => GTraversable' c (K1 i con) where
  gtraverse' f (K1 con) = K1 <$> f con

instance (GTraversable' c f) => GTraversable' c (M1 i meta f) where
  gtraverse' f (M1 inner) = M1 <$> gtraverse' @c f inner

instance (GTraversable' c f, GTraversable' c g) => GTraversable' c (f :+: g) where
  gtraverse' f (L1 val) = L1 <$> gtraverse' @c f val
  gtraverse' f (R1 val) = R1 <$> gtraverse' @c f val

instance (GTraversable' c f, GTraversable' c g) => GTraversable' c (f :*: g) where
  gtraverse' f (left :*: right)
    = (:*:) <$> gtraverse' @c f left <*> gtraverse' @c f right

instance (Traversable f, GTraversable' c g) => GTraversable' c (f :.: g) where
  gtraverse' f (Comp1 comp) = Comp1 <$> traverse (gtraverse' @c f) comp

instance
    {-# OVERLAPPABLE #-}
    (Generic a, GTraversable' c (Rep a))
  => GTraversable c a
  where
  gtraverse f val
    = to <$> gtraverse' @c f (from val)
