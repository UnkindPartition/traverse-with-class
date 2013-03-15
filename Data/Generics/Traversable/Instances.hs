{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, ConstraintKinds, UndecidableInstances #-}
-- | This module defines 'GTraversable' instances for standard types
-- exported by "Prelude"
module Data.Generics.Traversable.Instances () where

import Data.Generics.Traversable.Core
import Data.Generics.Traversable.TH
import Control.Applicative
import Data.Ratio

instance c () => GTraversable c ()
instance c Bool => GTraversable c Bool
instance c Int => GTraversable c Int
instance c Integer => GTraversable c Integer
instance c Float => GTraversable c Float
instance c Double => GTraversable c Double
instance c (Ratio n) => GTraversable c (Ratio n)
instance c Char => GTraversable c Char
instance c Ordering => GTraversable c Ordering

deriveGTraversable ''Maybe
deriveGTraversable ''Either
deriveGTraversable ''(,)
deriveGTraversable ''(,,)

-- Uniform instance for lists
instance (GTraversable c a, c a, c [a]) => GTraversable c [a] where
  gtraverse f = go where
    go [] = pure []
    go (x:xs) = (:) <$> f x <*> go xs

