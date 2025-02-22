{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GAD () where

import Control.Arrow.Constrained
import Control.Category.Constrained.Prelude
import qualified Control.Category.Hask as Hask
import Control.Monad.Constrained
import Data.Tagged
import Prelude ()

newtype D k a b = D (a -> (b, a `k` b))

linearD :: (a -> b) -> (a `k` b) -> D k a b
linearD f f' = D (\a -> (f a, f'))

instance (Category k) => Category (D k) where
  type Object (D k) a = Object k a
  id = linearD id id
  (D f) . (D g) =
    D
      $ \a ->
        let (ga, dg) = g a
            (fga, df) = f ga
         in (fga, df . dg)

instance (Cartesian k, UnitObject k ~ ()) => Cartesian (D k) where
  type PairObjects (D k) a b = PairObjects k a b
  swap = linearD swap swap
  attachUnit = linearD attachUnit attachUnit
  detachUnit = linearD detachUnit detachUnit
  regroup = linearD regroup regroup
  regroup' = linearD regroup' regroup'

instance (Morphism k, UnitObject k ~ ()) => Morphism (D k) where
  D f *** D g = D $ \(a, b) ->
    let (c, dc) = f a
        (c', dc') = g b
     in ((c, c'), dc *** dc')

instance (PreArrow k, UnitObject k ~ ()) => PreArrow (D k) where
  D f &&& D g = D $ \a ->
    let (b, db) = f a
        (c, dc) = g a
     in ((b, c), db &&& dc)
  terminal = linearD terminal terminal
  fst = linearD fst fst
  snd = linearD snd snd

instance (WellPointed k, UnitObject k ~ ()) => WellPointed (D k) where
  type PointObject (D k) x = PointObject k x
  unit = Tagged ()
  const x = linearD (const x) (const x)

infixl 6 ⊖, ⊕

class HasDelta a where
  type Delta a
  zero :: Delta a
  (⊕) :: a -> Delta a -> a
  (⊖) :: a -> a -> Delta a

instance (HasDelta a, HasDelta b) => HasDelta (a, b) where
  type Delta (a, b) = (Delta a, Delta b)
  (⊕) :: (a, b) -> Delta (a, b) -> (a, b)
  (a, b) ⊕ (da, db) = (a ⊕ da, b ⊕ db)
  (⊖) :: (a, b) -> (a, b) -> Delta (a, b)
  (a, b) ⊖ (da, db) = (a ⊖ da, b ⊖ db)
  zero = (zero @a, zero @b)

instance HasDelta () where
  type Delta () = ()
  () ⊕ () = ()
  () ⊖ () = ()
  zero = ()

newtype DelX a b = DelX (Delta a -> Delta b)

instance Category DelX where
  id = DelX id
  DelX f . DelX g = DelX (f . g)

instance Cartesian DelX where
  swap = DelX swap
  attachUnit = DelX attachUnit
  detachUnit = DelX detachUnit
  regroup = DelX regroup
  regroup' = DelX regroup'

instance Morphism DelX where
  DelX f *** DelX g = DelX (f *** g)

instance PreArrow DelX where
  DelX f &&& DelX g = DelX (f &&& g)
  terminal = DelX terminal
  fst = DelX fst
  snd = DelX snd

instance WellPointed DelX where
  type PointObject DelX x = HasDelta x
  unit = Tagged ()
  const :: forall x b. (Object DelX b, ObjectPoint DelX x) => x -> DelX b x
  const _ = DelX $ const (zero @x)

type Inc = D DelX

instance HasAgent Inc where
  alg = genericAlg
  ($~) = genericAgentMap

instance CartesianAgent Inc where
  alg1to2 = genericAlg1to2
  alg2to1 = genericAlg2to1
  alg2to2 = genericAlg2to2

instance (HasDelta x {-TODO: ??-}) => PointAgent (GenericAgent Inc) Inc a x where
  point = genericPoint