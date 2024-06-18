{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  $module
-- Copyright   :  (c) Powerweave
-- License     :  MIT
-- Portability :  portable

module Numeric.LinearProgramming (
    -- * Constructing linear programs
    LinearProgram(lpDirection, lpObjectiveFunction, lpConstraints, lpVariableBounds, lpAllVariables), linearProgram,
    Direction(..),
    
    -- * Linear functions
    LinearFunction(..), (*:), (+:), (-:), fromCoefficients, singleton, fromArgSet, constant, ones,
    coefficients, offsetBy,

    -- * Linear constraints
    LinearConstraint(..), boundedBy, eqTo,

    -- * Bounds
    Bounds, to, from, upto,

    -- * Solving linear programs
    solve, Solution(..), HiGHS.ModelStatus(..),
) where

import           Data.Bifunctor  ( second )
import           Data.Map        ( Map )
import qualified Data.Map        as Map
import qualified Data.Set        as Set
import qualified Data.Vector     as Vector
import qualified Data.Vector.Unboxed as Unboxed
import           Numeric.LinearProgramming.Program 
import           Numeric.LinearProgramming.HiGHS   ( HiGHSSolution(..) )
import qualified Numeric.LinearProgramming.HiGHS   as HiGHS


-- | Solution of a linear program, including the value 
-- of the objective function at the solution.
data Solution v c
    = MkSolution { objectiveValue :: c
                 , solution       :: Map v c
                 }
    deriving (Eq, Show)


-- | Solve a linear program.
-- In case the program is unsolvable, @Nothing@ is returned.
solve :: LinearProgram v Double -> Maybe (Solution v Double)
solve lp
    = let (objCoefficients, objOffset) = coefficients lp.lpObjectiveFunction
          (constraintMatrix, constraintBounds) = constraints lp
          coefficientBounds = Unboxed.fromListN (Set.size lp.lpAllVariables) $ map (snd . second (convertBounds 0) ) $ Map.toAscList (lpVariableBounds lp)
       in convert <$> HiGHS.optimize (lpDirection lp) objCoefficients objOffset constraintBounds coefficientBounds constraintMatrix
    where
        convert MkHiGHSSolution{..} = MkSolution { objectiveValue = solutionObjectiveValue
                                                 , solution = Map.fromDistinctAscList 
                                                            $ zip (Set.toAscList lp.lpAllVariables) 
                                                                  (Unboxed.toList solutionLocation)
                                                 }

-- HiGHS's threshold for 'infinity' is 1e20
infinity :: Fractional b => b
infinity = 1e20

convertBounds :: (Ord b, Fractional b) => b -> Bounds b -> (b, b)
convertBounds offset (Bounded mi ma) = ((max mi (-infinity)) - offset, (min ma infinity) - offset)
convertBounds offset (From mi)       = ((max mi (-infinity)) - offset, infinity)
convertBounds offset (UpTo ma)       = (-infinity, (min ma infinity) - offset)
convertBounds offset (EqTo lhs)      = (lhs - offset, lhs - offset)

constraints :: LinearProgram v Double -> (HiGHS.ConstraintMatrix, HiGHS.ConstraintBounds)
constraints lp = (constraintMatrix, constraintBounds)
    where
        constraintMatrix = Vector.map (\(MkLinearConstraint f _) -> fst $ coefficients f) (lpConstraints lp)
        constraintBounds = Vector.convert $ Vector.map (\(MkLinearConstraint (MkLinearFunction _ offset) b) -> convertBounds offset b) (lpConstraints lp)