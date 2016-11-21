{-# LANGUAGE ConstraintKinds, KindSignatures, MultiParamTypeClasses, RankNTypes, UndecidableInstances, ImplicitParams, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, CPP #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif

-- | All of the functions below work only on «interesting» subterms.
-- It is up to the instance writer to decide which subterms are
-- interesting and which subterms should count as immediate. This can
-- also depend on the context @c@.
--
-- The context, denoted @c@, is a constraint (of kind @* -> Constraint@)
-- that provides additional facilities to work with the data. Most
-- functions take an implicit parameter @?c :: p c@; it's
-- used to disambugate which context you are referring to. @p@ can be
-- @Proxy@ from the @tagged@ package or any other suitable type
-- constructor.
--
-- For more information, see:
--
-- [Scrap your boilerplate with class]
-- <http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/>
--
-- [Generalizing generic fold]
-- <http://ro-che.info/articles/2013-03-11-generalizing-gfoldl.html>

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
  , Rec
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
import Data.Proxy.Fork

import Data.Generics.Traversable.Core
import Data.Generics.Traversable.Instances ()

-- for documentation only
import Data.Foldable
import Data.Traversable

-- | 'Rec' enables \"deep traversals\".
--
-- It is satisfied automatically when its superclass constraints are
-- satisfied — you are not supposed to declare new instances of this class.
class    (GTraversable (Rec c) a, c a) => Rec (c :: * -> Constraint) a
instance (GTraversable (Rec c) a, c a) => Rec (c :: * -> Constraint) a

-- | Generic map over the immediate subterms
gmap
  :: (GTraversable c a, ?c :: p c)
  => (forall d . (c d) => d -> d)
  -> a -> a
gmap f = runIdentity . gtraverse (Identity . f)

-- | Generic monadic map over the immediate subterms
gmapM
  :: (Monad m, GTraversable c a, ?c :: p c)
  => (forall d . (c d) => d -> m d)
  -> a -> m a
gmapM f = unwrapMonad . gtraverse (WrapMonad . f)

-- | Generic monoidal fold over the immediate subterms (cf. 'foldMap' from
-- "Data.Foldable")
gfoldMap
  :: (Monoid r, GTraversable c a, ?c :: p c)
  => (forall d . (c d) => d -> r)
  -> a -> r
gfoldMap f = getConstant . gtraverse (Constant . f)

-- | Generic right fold over the immediate subterms
gfoldr
  :: (GTraversable c a, ?c :: p c)
  => (forall d . (c d) => d -> r -> r)
  -> r -> a -> r
gfoldr f z t = appEndo (gfoldMap (Endo . f) t) z

-- | Generic strict left fold over the immediate subterms
gfoldl'
  :: (GTraversable c a, ?c :: p c)
  => (forall d . (c d) => r -> d -> r)
  -> r -> a -> r
gfoldl' f z0 xs = gfoldr f' id xs z0
  where f' x k z = k $! f z x

-- | Apply a transformation everywhere in bottom-up manner
everywhere
  :: forall a c p .
     (Rec c a, ?c :: p c)
  => (forall d. (Rec c d) => d -> d)
  -> a -> a
everywhere f =
  let ?c = Proxy :: Proxy (Rec c) in
  let
    go :: forall a . Rec c a => a -> a
    go = f . gmap go
  in go

-- | Apply a transformation everywhere in top-down manner
everywhere'
  :: forall a c p .
     (Rec c a, ?c :: p c)
  => (forall d. (Rec c d) => d -> d)
  -> a -> a
everywhere' f =
  let ?c = Proxy :: Proxy (Rec c) in
  let
    go :: forall a . Rec c a => a -> a
    go = gmap go . f
  in go

-- | Monadic variation on everywhere
everywhereM
  :: forall m a c p .
     (Monad m, Rec c a, ?c :: p c)
  => (forall d. (Rec c d) => d -> m d)
  -> a -> m a
everywhereM f =
  let ?c = Proxy :: Proxy (Rec c) in
  let
    go :: forall a . Rec c a => a -> m a
    go = f <=< gmapM go
  in go

everything
  :: forall r a c p .
     (Rec c a, ?c :: p c)
  => (r -> r -> r)
  -> (forall d . (Rec c d) => d -> r)
  -> a -> r
everything combine f =
  let ?c = Proxy :: Proxy (Rec c) in
  let
    go :: forall a . Rec c a => a -> r
    go x = gfoldl' (\a y -> combine a (go y)) (f x) x
  in go

{- ^ Strict left fold over all elements, top-down

==== example use: test case
@
import Test.Hspec
import "Data.Generics.Traversable"
import "Language.Haskell.Exts.Syntax"
import "Data.Proxy"


main::IO()
main = hspec $ do
       describe \"Test.TestTraverseWithClass\" $ do
          it "case 1" $ do
            let ?c = Proxy::Proxy Update
            'everywhere'' upd qn1 `shouldBe` qn2
        where qn1 = Qual 1 (ModuleName 1 \"Mod\") (Ident 1 \"fn\")::QName Int
              qn2 = Qual 1 (ModuleName 1 \"NewMod\") (Ident 1 \"newFn\")::QName Int


instance 'GTraversable' ('Rec' Update) (QName l)

instance 'GTraversable' Update (QName l) where
    'gtraverse' fn0 a0 = fn0 a0


class Update a where
    upd::a -> a

instance Update (ModuleName l) where
    upd (ModuleName l0 mod0) = ModuleName l0 \"NewMod\"

instance Update (Name l) where
    upd (Ident l0 name0) = Ident l0 "newFn"
    upd other0 = other0

instance Update (QName l) where
    upd (Qual l0 mod0 name0) = Qual l0 mod1 name1
            where mod1 = upd mod0
                  name1 = upd name0
    upd (UnQual l0 name0) = UnQual l0 name1
            where name1 = upd name0
    upd other0 = other0
@   -}
