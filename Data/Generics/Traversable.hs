{-# LANGUAGE ConstraintKinds, KindSignatures, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
-- | All of the functions below work only on «interesting» subterms.
--
-- It is up to the instance writer to decide which subterms are
-- interesting and which subterms should count as immediate. This can
-- also depend on the context @c@.
--
-- The context, denoted @c@, is a constraint that provides additional
-- facilities to work with the data.

module Data.Generics.Traversable
  (
    -- * Open recursion combinators

    GTraversable(..)
  , gmap
  , gmapM
  , gfoldMap
  , gfoldr
  , gfoldl'

    -- * Closed recursion combinators
  , everywhere
  , everywhere'
  , everywhereM
  , everything
  )
  where

import GHC.Exts (Constraint)
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Functor.Identity
import Data.Functor.Constant

-- for documentation only
import Data.Foldable
import Data.Traversable


class GTraversable (c :: * -> Constraint) a where
  -- | Applicative traversal over (a subset of) immediate subterms. This is
  -- a generic version of 'traverse' from "Data.Traversable".
  --
  -- The supplied function is applied only to the «interesting» subterms.
  --
  -- Other subterms are lifted using 'pure', and the whole structure is
  -- folded back using '<*>'.
  gtraverse
    :: Applicative f
    => p c
    -> (forall d . (GTraversable c d, c d) => p c -> d -> f d)
    -> a -> f a

-- | Generic map over the immediate subterms
gmap
  :: GTraversable c a
  => p c
  -> (forall d . (GTraversable c d, c d) => p c -> d -> d)
  -> a -> a
gmap c f = runIdentity . gtraverse c (const $ Identity . f c)

-- | Generic monadic map over the immediate subterms
gmapM
  :: (Monad m, GTraversable c a)
  => p c
  -> (forall d . (GTraversable c d, c d) => p c -> d -> m d)
  -> a -> m a
gmapM c f = unwrapMonad . gtraverse c (const $ WrapMonad . f c)

-- | Generic monoidal fold over the immediate subterms (cf. 'foldMap' from
-- "Data.Foldable")
gfoldMap
  :: (Monoid r, GTraversable c a)
  => p c
  -> (forall d . (GTraversable c d, c d) => p c -> d -> r)
  -> a -> r
gfoldMap c f = getConstant . gtraverse c (const $ Constant . f c)

-- | Generic right fold over the immediate subterms
gfoldr
  :: GTraversable c a
  => p c
  -> (forall d . (GTraversable c d, c d) => p c -> d -> r -> r)
  -> r -> a -> r
gfoldr c f z t = appEndo (gfoldMap c (const $ Endo . f c) t) z

-- | Generic strict left fold over the immediate subterms
gfoldl'
  :: GTraversable c a
  => p c
  -> (forall d . (GTraversable c d, c d) => p c -> r -> d -> r)
  -> r -> a -> r
gfoldl' c f z0 xs = gfoldr c f' id xs z0
  where f' c x k z = k $! f c z x

-- | Apply a transformation everywhere in bottom-up manner
everywhere
  :: (GTraversable c a, c a)
  => p c
  -> (forall a. (GTraversable c a, c a) => p c -> a -> a)
  -> a -> a
everywhere c f = f c . gmap c (const $ everywhere c f)


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
