{-# LANGUAGE ConstraintKinds, KindSignatures, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
module Data.Generics.Traversable where

import GHC.Exts (Constraint)
import Control.Applicative
import Data.Monoid
import Data.Functor.Identity
import Data.Functor.Constant

class GTraversable (c :: * -> Constraint) a where
  gtraverse
    :: Applicative f
    => p c
    -> (forall d . (GTraversable c d, c d) => p c -> d -> f d)
    -> a -> f a

gmap
  :: GTraversable c a
  => p c
  -> (forall d . (GTraversable c d, c d) => p c -> d -> d)
  -> a -> a
gmap c f = runIdentity . gtraverse c (\c -> Identity . f c)

gmapM
  :: (Monad m, GTraversable c a)
  => p c
  -> (forall d . (GTraversable c d, c d) => p c -> d -> m d)
  -> a -> m a
gmapM c f = unwrapMonad . gtraverse c (\c -> WrapMonad . f c)

gfoldMap
  :: (Monoid r, GTraversable c a)
  => p c
  -> (forall d . (GTraversable c d, c d) => p c -> d -> r)
  -> a -> r
gfoldMap c f = getConstant . gtraverse c (\c -> Constant . f c)

gfoldr
  :: GTraversable c a
  => p c
  -> (forall d . (GTraversable c d, c d) => p c -> d -> r -> r)
  -> r -> a -> r
gfoldr c f z t = appEndo (gfoldMap c (\c -> Endo . f c) t) z

gfoldl'
  :: GTraversable c a
  => p c
  -> (forall d . (GTraversable c d, c d) => p c -> r -> d -> r)
  -> r -> a -> r
gfoldl' c f z0 xs = gfoldr c f' id xs z0
  where f' c x k z = k $! f c z x
