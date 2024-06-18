-----------------------------------------------------------------------------
-- |
-- Module      :  $module
-- Copyright   :  (c) Powerweave
-- License     :  MIT
-- Portability :  portable

module Numeric.LinearProgramming.HiGHS ( 
    highsVersion,
    ModelStatus(..),
    optimize,
    HiGHSSolution(..),

    ConstraintMatrix, ConstraintBounds, CoefficientBounds,

    -- * For testing purposes
    sparseRepresentation,
) where

import           Control.Exception      ( bracket )
import           Control.Monad          ( forM_ )
import           Data.Coerce            ( coerce )
import           Data.Vector            ( Vector )
import qualified Data.Vector            as Vector
import qualified Data.Vector.Unboxed    as Unboxed
import qualified Data.Vector.Storable   as Vector.Storable
import           Foreign.C.String       ( peekCString, newCString )
import           Foreign.C.Types        ( CInt(..), CDouble(..) )
import           Foreign.Marshal.Array  ( mallocArray, peekArray )
import           Foreign.Ptr            ( Ptr )
import           Numeric.LinearProgramming.Program        ( Direction(..) )
import qualified Numeric.LinearProgramming.HiGHS.LowLevel as LowLevel
import           System.IO.Unsafe       ( unsafePerformIO )


highsVersion :: String
highsVersion = unsafePerformIO $ peekCString LowLevel.c_highs_version
{-# NOINLINE highsVersion #-}


data ModelStatus
    = Notset
    | LoadError
    | ModelError
    | PresolveError
    | SolveError
    | PostsolveError
    | ModelEmpty
    | Optimal
    | Infeasible
    | UnboundedOrInfeasible
    | Unbounded
    | ObjectiveBound
    | ObjectiveTarget
    | TimeLimit
    | IterationLimit
    | Unknown
    | SolutionLimit
    | Interrupt
    deriving (Eq, Show, Enum, Ord)

type ObjectiveCoefficients = Unboxed.Vector Double 
type ConstraintBounds = Unboxed.Vector (Double, Double)
type CoefficientBounds = Unboxed.Vector (Double, Double)
type ConstraintMatrix = Vector (Unboxed.Vector Double) -- Vector of rows

data HiGHSSolution 
    = MkHiGHSSolution { solutionObjectiveValue :: Double
                      , solutionLocation       :: Unboxed.Vector Double
                      }
    deriving (Eq, Show)


direction :: Direction -> CInt
direction Maximize = LowLevel.c_highs_obj_sense_maximize
direction Minimize = LowLevel.c_highs_obj_sense_minimize

-- This illustrates the use of Highs_lpCall, the simple C interface to
-- HiGHS. It's designed to solve the general LP problem
--
-- Min c^Tx + d subject to L <= Ax <= U; l <= x <= u
--
-- where A is a matrix with m rows and n columns
--
-- The scalar n is num_col
-- The scalar m is num_row
--
-- The vector c is col_cost
-- The scalar d is offset
-- The vector l is col_lower
-- The vector u is col_upper
-- The vector L is row_lower
-- The vector U is row_upper
--
-- The matrix A is represented in packed vector form, either
-- row-wise or column-wise: only its nonzeros are stored
--
-- * The number of nonzeros in A is num_nz
--
-- * The indices of the nonnzeros in the vectors of A are stored in a_index
--
-- * The values of the nonnzeros in the vectors of A are stored in a_value
--
-- * The position in a_index/a_value of the index/value of the first
-- nonzero in each vector is stored in a_start
--
-- Note that a_start[0] must be zero
--
-- After a successful call to Highs_lpCall, the primal and dual
-- solution, and the simplex basis are returned as follows
--
-- The vector x is col_value
-- The vector Ax is row_value
-- The vector of dual values for the variables x is col_dual
-- The vector of dual values for the variables Ax is row_dual
-- The basic/nonbasic status of the variables x is col_basis_status
-- The basic/nonbasic status of the variables Ax is row_basis_status
--
-- The status of the solution obtained is model_status
--
-- The use of Highs_lpCall is illustrated for the LP
--
-- Min    f  =  x_0 +  x_1 + 3
-- s.t.                x_1 <= 7
--        5 <=  x_0 + 2x_1 <= 15
--        6 <= 3x_0 + 2x_1
-- 0 <= x_0 <= 4; 1 <= x_1
--
-- Although the first constraint could be expressed as an upper
-- bound on x_1, it serves to illustrate a non-trivial packed
-- column-wise matrix.
--
optimize :: Direction
         -> ObjectiveCoefficients -- ^ Multiplicative coefficients
         -> Double                -- ^ Objective offset
         -> ConstraintBounds      -- ^ Bounds on constraints
         -> CoefficientBounds
         -> ConstraintMatrix
         -> Maybe HiGHSSolution
optimize dir objCoeffs objOffset constraintBounds coeffBounds constraintMatrix 
    = let num_col = Unboxed.length objCoeffs
          num_row = Unboxed.length constraintBounds
          (constraintLowerBounds, constraintUpperBounds) = Unboxed.unzip constraintBounds
          (coeffsLowerBounds, coeffsUpperBounds) = Unboxed.unzip coeffBounds
          (numNonZero, aStart, aIndex, aValues) = sparseRepresentation constraintMatrix
       in unsafePerformIO $
        Vector.Storable.unsafeWith (Vector.Storable.unsafeCoerceVector $ Vector.convert objCoeffs) $ \(col_cost :: Ptr CDouble) -> do
            Vector.Storable.unsafeWith (Vector.Storable.unsafeCoerceVector $ Vector.convert constraintLowerBounds) $ \(row_lower :: Ptr CDouble) -> do
                Vector.Storable.unsafeWith (Vector.Storable.unsafeCoerceVector $ Vector.convert constraintUpperBounds) $ \(row_upper :: Ptr CDouble) -> do
                    Vector.Storable.unsafeWith (Vector.Storable.unsafeCoerceVector $ Vector.convert coeffsLowerBounds) $ \(col_lower :: Ptr CDouble) -> do
                        Vector.Storable.unsafeWith (Vector.Storable.unsafeCoerceVector $ Vector.convert coeffsUpperBounds) $ \(col_upper :: Ptr CDouble) -> do
                            Vector.Storable.unsafeWith (Vector.Storable.unsafeCoerceVector $ Vector.convert aStart) $ \(a_start :: Ptr CInt) -> do
                                Vector.Storable.unsafeWith (Vector.Storable.unsafeCoerceVector $ Vector.convert aIndex) $ \(a_index :: Ptr CInt) -> do
                                    Vector.Storable.unsafeWith (Vector.Storable.unsafeCoerceVector $ Vector.convert aValues) $ \(a_values :: Ptr CDouble) -> do
                                        
                                        (col_value :: Ptr CDouble) <- mallocArray num_col
                                        (col_dual :: Ptr CDouble)  <- mallocArray num_col
                                        (row_value :: Ptr CDouble) <- mallocArray num_row
                                        (row_dual :: Ptr CDouble)  <- mallocArray num_row

                                        bracket LowLevel.c_highs_create LowLevel.c_highs_destroy $ \highs -> do
                                            
                                            -- The following lines are required to minimize the amount of
                                            -- garbage that HiGHS outputs to standard output.
                                            forM_ ["output_flag", "log_to_console"] $ \flag -> do
                                                flagname <- newCString flag
                                                LowLevel.c_highs_set_bool_option_value highs flagname False
                                            
                                            -- We are using HiGHS's full API, rather than the 'simple'
                                            -- API via Highs_lpCall, because using the latter over and over
                                            -- would sometimes result in corrupted memory. 
                                            pass_status <- LowLevel.c_highs_passLp highs
                                                                           (fromIntegral num_col) 
                                                                           (fromIntegral num_row) 
                                                                           (fromIntegral numNonZero) 
                                                                           LowLevel.c_highs_matrix_format_rowwise 
                                                                           (direction dir)
                                                                           (coerce objOffset) 
                                                                           col_cost 
                                                                           col_lower 
                                                                           col_upper 
                                                                           row_lower 
                                                                           row_upper
                                                                           a_start 
                                                                           a_index 
                                                                           a_values

                                            if pass_status /= LowLevel.c_highs_status_ok
                                                then pure Nothing
                                                else do
                                                    run_status <- LowLevel.c_highs_run highs
                                                    if run_status /= LowLevel.c_highs_status_ok
                                                        then pure Nothing
                                                        else do
                                                            modelStatus <- (toEnum . fromIntegral) <$> LowLevel.c_highs_get_model_status highs
                                                            case modelStatus of
                                                                Optimal -> do
                                                                    LowLevel.c_highs_get_solution highs col_value col_dual row_value row_dual

                                                                    colValue <- Vector.convert 
                                                                            . Vector.map coerce 
                                                                            . Vector.convert 
                                                                            . Vector.Storable.fromListN num_col <$> peekArray num_col col_value
                                                                    
                                                                    pure $ Just $ MkHiGHSSolution { solutionObjectiveValue = (Unboxed.sum $ Unboxed.zipWith (*) colValue objCoeffs) + objOffset
                                                                                                  , solutionLocation       = colValue
                                                                                                  }
                                                                -- In case the model status isn't Optimal (e.g. Infeasible), 
                                                                -- HiGHS will still return a "solution" which is completely nonsensical
                                                                _ -> pure Nothing
{-# NOINLINE optimize #-}


sparseRepresentation :: ConstraintMatrix -> (Int, Vector CInt, Vector CInt, Unboxed.Vector Double)
sparseRepresentation vs =
            let nonZeroValues = Vector.convert $ Vector.concatMap (Vector.convert . Unboxed.filter (/= 0)) vs
                nonZeroValuesPerColumn = Vector.map (Unboxed.length . Unboxed.filter (/= 0)) vs
             in ( Unboxed.length nonZeroValues
                -- The next quantity was hard to understand for me, so here's an example.
                -- Consider the following constraint matrix:
                --    0  1
                --    1  2
                --    3  2
                -- HiGHS expect the matrix above to be distilled into three arrays:
                -- * the values array is [1,3,1,2,2]
                -- * The index array is [1,2,0,1,2], which represents the indices of non-zero values from the respective columns
                -- * the start array describes at which point do the values in [1,3,1,2,2] switch columns.
                --   In this case, the array is [0, 2], because the nonzero values of the second column ( [1,2,2] ) start at index 2
                --   of the values and index arrays.
                -- Another example:
                --   0 1 0
                --   3 0 0
                --   0 7 9
                -- becomes
                -- * values: [3,1,7,9]
                -- * index: [1,0,2,2]
                -- * start:  [0,1,3]
                , Vector.map fromIntegral $ Vector.prescanl' (+) 0 nonZeroValuesPerColumn -- a_start
                , Vector.concatMap (Vector.map (fromIntegral . fst) . Vector.convert . Unboxed.filter (\(_, val) -> val /= 0) . Unboxed.indexed) vs
                -- ^ a_index
                , nonZeroValues
                -- ^ a_value
                )
{-# INLINEABLE sparseRepresentation #-}