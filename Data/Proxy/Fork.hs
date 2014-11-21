{-# LANGUAGE ConstraintKinds, KindSignatures, PolyKinds #-}
-- | In @tagged@, PolyKinds are enabled only starting with 7.6. Hence we
-- have our own Proxy.
module Data.Proxy.Fork where

import GHC.Exts (Constraint)

data Proxy (c :: * -> Constraint) = Proxy
