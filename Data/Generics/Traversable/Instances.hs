{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, ConstraintKinds, UndecidableInstances #-}
-- | This module defines 'GTraversable' instances for standard types
-- exported by "Prelude"
module Data.Generics.Traversable.Instances () where

import Data.Generics.Traversable
import Data.Generics.Traversable.TH
import Control.Applicative

instance GTraversable c ()
instance GTraversable c Bool
instance GTraversable c Int
instance GTraversable c Integer
instance GTraversable c Float
instance GTraversable c Double
instance GTraversable c Char
instance GTraversable c Ordering

deriveGTraversable ''Maybe
deriveGTraversable ''Either
deriveGTraversable ''(,)
deriveGTraversable ''(,,)
deriveGTraversable ''(,,,)
deriveGTraversable ''(,,,,)
deriveGTraversable ''(,,,,,)
deriveGTraversable ''(,,,,,,)
deriveGTraversable ''(,,,,,,,)

-- Uniform instance for lists
instance (GTraversable c a, c a) => GTraversable c [a] where
  gtraverse f = go where
    go [] = pure []
    go (x:xs) = (:) <$> f x <*> go xs

