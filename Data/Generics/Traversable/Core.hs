{-# LANGUAGE ConstraintKinds, KindSignatures, MultiParamTypeClasses, RankNTypes, UndecidableInstances, ImplicitParams #-}
module Data.Generics.Traversable.Core where

import GHC.Exts (Constraint)
import Control.Applicative

class GTraversable (c :: * -> Constraint) a where
  -- | Applicative traversal over (a subset of) immediate subterms. This is
  -- a generic version of 'traverse' from "Data.Traversable".
  --
  -- The supplied function is applied only to the «interesting» subterms.
  --
  -- Other subterms are lifted using 'pure', and the whole structure is
  -- folded back using '<*>'.
  --
  -- 'gtraverse' has a default implementation @const pure@, which works for
  -- types without interesting subterms (in particular, atomic types).
  gtraverse
    :: (Applicative f, ?c :: p c)
    => (forall d . (GTraversable c d, c d) => d -> f d)
    -> a -> f a
  gtraverse = const pure

