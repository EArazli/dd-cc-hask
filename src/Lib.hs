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
import Data.Bifunctor (bimap)
import Data.Tagged
import Prelude ()

infixl 6 ⊖, ⊕

class HasDelta a where
  type D a
  (⊕) :: a -> D a -> a
  (⊖) :: a -> a -> D a

instance (HasDelta a, HasDelta b) => HasDelta (a, b) where
  type D (a, b) = (D a, D b)
  (⊕) :: (a, b) -> D (a, b) -> (a, b)
  (a, b) ⊕ (da, db) = (a ⊕ da, b ⊕ db)
  (⊖) :: (a, b) -> (a, b) -> D (a, b)
  (a, b) ⊖ (da, db) = (a ⊖ da, b ⊖ db)

instance HasDelta () where
  type D () = ()
  () ⊕ () = ()
  () ⊖ () = ()

newtype Inc x y = Inc (x -> (y, D x -> D y))

instance Category Inc where
  id :: Inc a a
  id = Inc (,id)
  (.) :: Inc b c -> Inc a b -> Inc a c
  Inc f . Inc g = Inc $ \x -> case g x of
    (gx, dg) -> case f gx of
      (fgx, df) -> (fgx, df . dg)

instance Cartesian Inc where
  type UnitObject Inc = ()
  swap = Inc $ \(a, b) -> ((b, a), \(ax, bx) -> (bx, ax))
  attachUnit = Inc $ \a -> ((a, ()), (,()))
  detachUnit = Inc $ \(a, _) -> (a, fst)
  regroup = Inc $ \(a, (b, c)) -> (((a, b), c), \(ax, (bx, cx)) -> ((ax, bx), cx))
  regroup' = Inc $ \((a, b), c) -> ((a, (b, c)), \((ax, bx), cx) -> (ax, (bx, cx)))

instance Morphism Inc where
  Inc f *** Inc g = Inc $ \(a, b) ->
    let (c, dc) = f a
        (d, dd) = g b
     in ((c, d), bimap dc dd)

instance PreArrow Inc where
  Inc f &&& Inc g = Inc $ \a ->
    let (b, db) = f a
        (c, dc) = g a
     in ((b, c), \x -> (db x, dc x))
  terminal = Inc $ const ((), const ())
  fst = Inc $ \(x, _) -> (x, fst)
  snd = Inc $ \(_, y) -> (y, snd)

instance WellPointed Inc where
  type PointObject Inc x = HasDelta x
  unit :: CatTagged Inc (UnitObject Inc)
  unit = Tagged ()
  const :: (Object Inc b, ObjectPoint Inc x) => x -> Inc b x
  const x = Inc $ const (x, const $ x ⊖ x)