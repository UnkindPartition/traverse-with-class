{-# LANGUAGE ConstraintKinds, KindSignatures, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
module Data.Generics.Traversable where

import GHC.Exts (Constraint)
import Control.Applicative
import Data.Monoid
import Data.Functor.Identity
import Data.Functor.Constant

class ctx a => GTraversable (ctx :: * -> Constraint) a where
  gtraverse
    :: Applicative f
    => proxy ctx
    -> (forall d . GTraversable ctx d => d -> f d)
    -> a -> f d

gmap
  :: GTraversable ctx a
  => proxy ctx
  -> (forall d . GTraversable ctx d => d -> d)
  -> a -> a
gmap ctx f = runIdentity . gtraverse ctx (Identity . f)

gmapM
  :: (Monad m, GTraversable ctx a)
  => proxy ctx
  -> (forall d . GTraversable ctx d => d -> m d)
  -> a -> m a
gmapM ctx f = unwrapMonad . gtraverse ctx (WrapMonad . f)

gfoldMap
  :: (Monoid r, GTraversable ctx a)
  => proxy ctx
  -> (forall d . GTraversable ctx d => d -> r)
  -> a -> r
gfoldMap ctx f = getConstant . gtraverse ctx (Constant . f)

gfoldr
  :: GTraversable ctx a
  => proxy ctx
  -> (forall d . (GTraversable ctx d) => d -> r -> r)
  ->                                     r -> a -> r
gfoldr ctx f z t = appEndo (gfoldMap ctx (Endo . f) t) z

gfoldl'
  :: GTraversable ctx a
  => proxy ctx
  -> (forall d . (GTraversable ctx d) => r -> d -> r)
  ->                                     r -> a -> r
gfoldl' ctx f z0 xs = gfoldr ctx f' id xs z0
  where f' x k z = k $! f z x
