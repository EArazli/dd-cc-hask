{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Lib () where

import Control.Arrow.Constrained
import Control.Category.Constrained.Prelude
import qualified Control.Category.Hask as Hask
import Control.Monad.Constrained
import Prelude ()

newtype Delta x = Delta x
  deriving (Show)

newtype Inc x y = Inc (x -> (y, Delta x -> Delta y))

instance Category Inc where
  id :: Inc a a
  id = Inc (,id)
  (.) :: Inc b c -> Inc a b -> Inc a c
  Inc f . Inc g = Inc $ \x -> case g x of
    (gx, dg) -> case f gx of
      (fgx, df) -> (fgx, df . dg)

instance Cartesian Inc where
  swap = Inc $ \(a, b) -> ((b, a), \(Delta (ax, bx)) -> Delta (bx, ax))
  attachUnit = Inc $ \a -> ((a, ()), \(Delta ax) -> Delta (ax, ()))
  detachUnit = Inc $ \(a, _) -> (a, \(Delta (ax, _)) -> Delta ax)
  regroup = Inc $ \(a, (b, c)) -> (((a, b), c), \(Delta (ax, (bx, cx))) -> Delta ((ax, bx), cx))
  regroup' = Inc $ \((a, b), c) -> ((a, (b, c)), \(Delta ((ax, bx), cx)) -> Delta (ax, (bx, cx)))

instance Morphism Inc where
  Inc f *** Inc g = Inc $ \(b, c) -> case f b of
    (b', db') -> case g c of
      (c', dc') -> ((b', c'), \(Delta (x, y)) -> Delta (_, _))
