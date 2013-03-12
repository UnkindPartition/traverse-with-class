{-# LANGUAGE ConstraintKinds, KindSignatures, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
module Data.Generics.Traversable where

import GHC.Exts (Constraint)
import Control.Applicative
import Data.Monoid
import Data.Functor.Identity
import Data.Functor.Constant

class GTraversable (ctx :: * -> Constraint) a where
  gtraverse
    :: Applicative f
    => proxy ctx
    -> (forall d . (GTraversable ctx d, ctx d) => proxy ctx -> d -> f d)
    -> a -> f a

gmap
  :: GTraversable ctx a
  => proxy ctx
  -> (forall d . (GTraversable ctx d, ctx d) => proxy ctx -> d -> d)
  -> a -> a
gmap ctx f = runIdentity . gtraverse ctx (\ctx -> Identity . f ctx)

gmapM
  :: (Monad m, GTraversable ctx a)
  => proxy ctx
  -> (forall d . (GTraversable ctx d, ctx d) => proxy ctx -> d -> m d)
  -> a -> m a
gmapM ctx f = unwrapMonad . gtraverse ctx (\ctx -> WrapMonad . f ctx)

gfoldMap
  :: (Monoid r, GTraversable ctx a)
  => proxy ctx
  -> (forall d . (GTraversable ctx d, ctx d) => proxy ctx -> d -> r)
  -> a -> r
gfoldMap ctx f = getConstant . gtraverse ctx (\ctx -> Constant . f ctx)

gfoldr
  :: GTraversable ctx a
  => proxy ctx
  -> (forall d . (GTraversable ctx d, ctx d) => proxy ctx -> d -> r -> r)
  -> r -> a -> r
gfoldr ctx f z t = appEndo (gfoldMap ctx (\ctx -> Endo . f ctx) t) z

gfoldl'
  :: GTraversable ctx a
  => proxy ctx
  -> (forall d . (GTraversable ctx d, ctx d) => proxy ctx -> r -> d -> r)
  -> r -> a -> r
gfoldl' ctx f z0 xs = gfoldr ctx f' id xs z0
  where f' ctx x k z = k $! f ctx z x
