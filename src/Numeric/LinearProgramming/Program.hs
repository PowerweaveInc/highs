-----------------------------------------------------------------------------
-- |
-- Module      :  $module
-- Copyright   :  (c) Powerweave
-- License     :  MIT
-- Portability :  portable

module Numeric.LinearProgramming.Program (
    LinearProgram(lpDirection, lpObjectiveFunction, lpConstraints, lpVariableBounds, lpAllVariables), linearProgram,
    Direction(..),
    
    -- * Linear functions
    LinearFunction(..), (*:), (+:), (-:), fromCoefficients, fromArgSet, ones, constant, singleton, offsetBy,
    coefficients,

    -- * Linear constraints
    LinearConstraint(..), boundedBy, eqTo,

    -- * Bounds
    Bounds(..), to, from, upto,
) where

import qualified Control.Foldl       as Fold
import qualified Data.List           ( intercalate )
import           Data.Map.Strict     ( Map )
import qualified Data.Map.Strict     as Map
import           Data.Semigroup      ( Arg )
import           Data.Set            ( Set )
import qualified Data.Set            as Set
import           Data.Vector.Unboxed ( Unbox, Vector )
import qualified Data.Vector         as Boxed
import qualified Data.Vector.Unboxed as Vector


infix 9 `to`
infix 4 `boundedBy`
infix 4 `eqTo`
infixl 6 +:, -:
infixl 7 *:


-- | A linear function is a linear combination of variables.
-- Variables are labeled using the type @v@, while the coefficients are 
-- of type @c@.
data LinearFunction v c = MkLinearFunction (Map v c) c
    deriving (Eq, Functor)

instance (Show v, Show c, Num c, Eq c) => Show (LinearFunction v c) where
    show (MkLinearFunction m offset) 
        = let tail' = if offset == 0 then ""  else " + " <> show offset 
           in (Data.List.intercalate " + " $ [ show c <> " " <> show v | (v, c) <- Map.toAscList m]) <> tail'


instance (Num c, Ord v) => Semigroup (LinearFunction v c) where
    (<>) = (+:)

instance (Num c, Ord v) => Monoid (LinearFunction v c) where
    mempty = MkLinearFunction mempty 0

-- | Construct a linear function from some containers of key-value pairs.
-- If the key-value pairs contain duplicate keys, the coefficients are added:
--
-- >>> fromCoefficients [('a', 1), ('a', 2), ('b', 5)]
-- 3 'a' + 5 'b'
fromCoefficients :: (Ord v, Num c, Foldable f) => f (v, c) -> LinearFunction v c
fromCoefficients = flip MkLinearFunction 0 . Fold.fold (Fold.foldByKeyMap Fold.sum)
{-# INLINEABLE fromCoefficients #-}


-- | Construct a linear function a set of of key-value pairs,
-- where the keys are known to be unique and pre-sorted in ascending order
-- by using the 'Arg' newtype wrapper.
--
-- You should prefer to use 'fromCoefficients' in every situation
fromArgSet :: Num c => Set (Arg v c) -> LinearFunction v c
fromArgSet = flip MkLinearFunction 0 . Map.fromArgSet


-- | Add an offset to a linear equationvalue pairs.
--
-- >>> (3 *: 'a' +: 2 *: 'b') `offsetBy` 5
-- 3 'a' + 2 'b' + 5
offsetBy :: Num c => LinearFunction v c -> c -> LinearFunction v c
offsetBy (MkLinearFunction m o) offset = MkLinearFunction m (o + offset)


-- | Create a linear function with a single variable
-- 
-- >>> singleton 2 'b'
-- 2 'b'
singleton :: Num c => c -> v -> LinearFunction v c
singleton coeff var = MkLinearFunction (Map.singleton var coeff) 0


-- | Construct a linear function from some containers of labels, where
-- the polynomial coefficients are all one. 
-- If the key-value pairs contain duplicate keys, the coefficients are added:
--
-- >>> ones ['a', 'b', 'c', 'a']
-- 2 'a' + 1 'b' + 1 'c'
ones :: (Ord v, Num c, Foldable f, Functor f) => f v -> LinearFunction v c
ones = constant 1


-- | Construct a linear function from some containers of labels, where
-- the polynomial coefficients are all the same constant. 
-- If the key-value pairs contain duplicate keys, the coefficients are added:
--
-- >>> constant 3 ['a', 'b', 'c', 'a']
-- 6 'a' + 3 'b' + 3 'c'
constant :: (Ord v, Num c, Foldable f, Functor f) => c -> f v -> LinearFunction v c
constant cst = fromCoefficients . fmap (,cst)


-- | Return the coefficients of the linear function
-- in increasing order of variables, and its offset.
-- >>> coefficients $ (1.0 *: "y") +: (2.0 *: "z") +: (3.0 *: "x")
-- [3.0,1.0,2.0]
coefficients :: Unbox c => LinearFunction v c -> (Vector c, c)
coefficients (MkLinearFunction m offset) = (Vector.fromListN (Map.size m) (Map.elems m), offset)
{-# SPECIALIZE coefficients :: LinearFunction v Double -> (Vector Double, Double) #-}


-- | Build a single term of a linear function:
--
-- >>> 1.0 *: "x"
-- 1.0 "x"
(*:) :: Num c => c -> v -> LinearFunction v c
(*:) coeff label = MkLinearFunction (Map.singleton label coeff) 0


-- | Combine linear functions to build larger linear functions
--
-- >>> 1.0 *: 'x' +: 2.0 *: 'y' +: 3.0 *: 'z'
-- 1.0 'x' + 2.0 'y' + 3.0 'z'
(+:) :: (Num c, Ord v) => LinearFunction v c -> LinearFunction v c -> LinearFunction v c
(+:) (MkLinearFunction m1 offset1) (MkLinearFunction m2 offset2) = MkLinearFunction (Map.unionWith (+) m1 m2) (offset1 + offset2)


-- | Combine linear functions to build larger linear functions
--
-- >>> 1.0 *: 'x' +: 2.0 *: 'y' -: 3.0 *: 'z'
-- 1.0 'x' + 2.0 'y' + -3.0 'z'
(-:) :: (Num c, Ord v) => LinearFunction v c -> LinearFunction v c -> LinearFunction v c
(-:) left right = left +: (fmap (negate) right)


-- | A constraint is a constraint on the bounds of a linear combination of variables.
-- Variables are labeled using the type @v@, while the coefficients are 
-- of type @c@.
data LinearConstraint v c = MkLinearConstraint (LinearFunction v c) (Bounds c)

instance (Show v, Show c, Num c, Eq c) => Show (LinearConstraint v c) where
    show (MkLinearConstraint m (Bounded lb ub)) = mconcat [ show lb, " ≤ ", show m, " ≤ ", show ub ]
    show (MkLinearConstraint m (From    lb   )) = mconcat [ show lb, " ≤ ", show m ]
    show (MkLinearConstraint m (UpTo       ub)) = mconcat [                 show m, " ≤ ", show ub ]
    show (MkLinearConstraint m (EqTo      rhs)) = mconcat [ show m, " = ", show rhs ]

instance Functor (LinearConstraint v) where
    fmap f (MkLinearConstraint linfunc bds) = MkLinearConstraint (fmap f linfunc) (fmap f bds)


-- | Datatype representing an /inclusive/ Bounds of keys, which can either be bounded
-- or unbounded. The canonical ways to construct a 'Bounds' are to use 'to', 'from', and 'upto':
data Bounds k 
    = Bounded k k
    | From k
    | UpTo k
    | EqTo k
    deriving (Eq, Functor)

instance Show k => Show (Bounds k) where
    show (Bounded start stop) = mconcat ["Bounds (from ", show start, " to ", show stop, ")"]
    show (From    start     ) = mconcat ["Bounds (from ", show start, ")"]
    show (UpTo          stop) = mconcat ["Bounds (up to ", show stop, ")"]
    show (EqTo           lhs) = mconcat ["Equal to ", show lhs]


-- | Create a bounded 'Bounds'.
to :: Ord k => k -> k -> Bounds k
to k1 k2 = Bounded (min k1 k2) (max k1 k2)


-- | Create an unbounded 'Bounds'.
from :: k -> Bounds k
from = From


-- | Create an unbounded 'Bounds'.
upto :: k -> Bounds k
upto = UpTo


-- | Create a 'LinearConstraint' by bounding the value of a 'LinearFunction'.
--
-- >>> 1 *: 'x' +: 3 *: 'y' `boundedBy` 0 `to` 10
-- 0 ≤ 1 'x' + 3 'y' ≤ 10
boundedBy :: LinearFunction v c -> Bounds c -> LinearConstraint v c
boundedBy = MkLinearConstraint


-- | Create a 'LinearConstraint' by equating the value of a 'LinearFunction'.
--
-- >>> 1 *: 'x' +: 3 *: 'y' `eq` 0
-- 0 ≤ 1 'x' + 3 'y' = 0
eqTo :: LinearFunction v c -> c -> LinearConstraint v c
eqTo f = MkLinearConstraint f . EqTo


data Direction 
    = Minimize 
    | Maximize
    deriving (Show, Eq, Ord, Enum, Bounded)


data LinearProgram v c
    = MkLinearProgram { lpDirection         :: Direction
                      , lpObjectiveFunction :: LinearFunction v c 
                      , lpConstraints       :: Boxed.Vector (LinearConstraint v c)
                      , lpVariableBounds    :: Map v (Bounds c)
                      , lpAllVariables      :: Set v
                      }
    deriving (Show)

-- | Construct a linear program, ensuring that every component
-- represents all possible variables.
linearProgram :: (Ord v) 
              => Direction
              -> LinearFunction v Double
              -> Boxed.Vector (LinearConstraint v Double)
              -> Map v (Bounds Double)
              -> LinearProgram v Double
linearProgram dir obj@(MkLinearFunction m1 _) constraints bounds
    = MkLinearProgram { lpDirection         = dir
                      , lpObjectiveFunction = normalizeLinearFunction obj
                      , lpConstraints       = Boxed.map (mapLinearFunction normalizeLinearFunction) constraints
                      , lpVariableBounds    = bounds `Map.union` Map.fromSet (const ((-1/0) `to` (1/0))) allVariables
                      , lpAllVariables      = allVariables
                      }
    where
        allVariables = mconcat [ Map.keysSet m1 
                               , Map.keysSet bounds
                               , Set.unions (Boxed.map (\(MkLinearConstraint (MkLinearFunction m2 _) _) -> Map.keysSet m2) constraints)
                               ]
        normalizeLinearFunction lin = lin <> MkLinearFunction (Map.fromSet (const 0) allVariables) 0
        mapLinearFunction f (MkLinearConstraint lin bds) = MkLinearConstraint (f lin) bds
