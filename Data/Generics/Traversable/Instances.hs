{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, ConstraintKinds, UndecidableInstances #-}
-- | This module defines 'GTraversable' instances for standard types
-- exported by "Prelude"
module Data.Generics.Traversable.Instances () where

import Data.Generics.Traversable.Core
import Data.Generics.Traversable.TH
import Control.Applicative
import Data.Ratio

instance GTraversable c ()
instance GTraversable c Bool
instance GTraversable c Int
instance GTraversable c Integer
instance GTraversable c Float
instance GTraversable c Double
instance GTraversable c (Ratio n)
instance GTraversable c Char
instance GTraversable c Ordering

--deriveGTraversable ''Maybe
instance c_0 a_1 => Data.Generics.Traversable.Core.GTraversable c_0
                                                                (Maybe a_1)
    where gtraverse = \f_2 x_3 -> case x_3 of
                                                                     Nothing -> Control.Applicative.pure Nothing
                                                                     Just arg_4 -> (Control.Applicative.<*>) (Control.Applicative.pure Just) (f_2 arg_4)


-- Uniform instance for lists
instance c a => GTraversable c [a] where
  gtraverse f = go where
    go [] = pure []
    go (x:xs) = (:) <$> f x <*> go xs

