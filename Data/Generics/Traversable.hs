{-# LANGUAGE ConstraintKinds, KindSignatures, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
module Data.Generics.Traversable where

import GHC.Exts (Constraint)
import Control.Applicative
import Control.Monad
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

-- | Apply a transformation everywhere in bottom-up manner
everywhere
  :: (forall a. (GTraversable c a, c a) => p c -> a -> a)
  -> (forall a. (GTraversable c a, c a) => p c -> a -> a)
everywhere f c = f c . gmap c (everywhere f)


-- | Apply a transformation everywhere in top-down manner
everywhere'
  :: (GTraversable c a, c a)
  => p c
  -> (forall a. (GTraversable c a, c a) => p c -> a -> a)
  -> a -> a
everywhere' c f = gmap c (const $ everywhere' c f) . f c

-- | Monadic variation on everywhere
everywhereM
  :: (Monad m, GTraversable c a, c a)
  => p c
  -> (forall a. (GTraversable c a, c a) => p c -> a -> m a)
  -> a -> m a
everywhereM c f = f c <=< gmapM c (const $ everywhereM c f)

-- | Strict left fold over all elements, top-down
everything
  :: (GTraversable c a, c a)
  => p c
  -> (r -> r -> r)
  -> (forall d . (GTraversable c d, c d) => p c -> d -> r)
  -> a -> r
everything c combine f x =
  gfoldl' c (\_ a y -> combine a (everything c combine f y)) (f c x) x
