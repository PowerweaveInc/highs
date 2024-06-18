{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- In order to clearly distinguish low-level imported
-- code vs. Haskell code, we use snake_case for foreign concepts
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  $module
-- Copyright   :  (c) Powerweave
-- License     :  MIT
-- Portability :  portable

module Numeric.LinearProgramming.HiGHS.LowLevel ( 
    c_highs_version,
    -- * Handling a Highs instance

    -- * Status codes
    c_highs_status_error,
    c_highs_status_ok,
    c_highs_status_warning,

    -- * Matrix format
    c_highs_matrix_format_colwise,
    c_highs_matrix_format_rowwise,

    -- * Objective sense direction
    c_highs_obj_sense_minimize,
    c_highs_obj_sense_maximize,

    -- * Model statuses
    c_highs_model_status_Notset, c_highs_model_status_LoadError, c_highs_model_status_ModelError,
    c_highs_model_status_PresolveError, c_highs_model_status_SolveError, c_highs_model_status_PostsolveError,
    c_highs_model_status_ModelEmpty, c_highs_model_status_Optimal, c_highs_model_status_Infeasible, 
    c_highs_model_status_UnboundedOrInfeasible, c_highs_model_status_Unbounded, 
    c_highs_model_status_ObjectiveBound, c_highs_model_status_ObjectiveTarget, c_highs_model_status_TimeLimit, 
    c_highs_model_status_IterationLimit, c_highs_model_status_Unknown, c_highs_model_status_SolutionLimit, 
    c_highs_model_status_Interrupt,

    -- * Solving general linear programs
    c_highs_lpcall,

    -- * Full API
    c_highs_create,
    c_highs_destroy,
    c_highs_passLp,
    c_highs_set_bool_option_value,
    c_highs_run,
    c_highs_get_model_status,
    c_highs_get_solution,
) where

import           Foreign          ( Ptr )
import           Foreign.C.String ( CString )
import           Foreign.C.Types  ( CInt(..), CDouble(..) )


foreign import capi "interfaces/highs_c_api.h Highs_version" c_highs_version :: CString


-------------------------------------------------------------------------------
-- HiGHS status codes

type Status = CInt 

-- | These constants are defined in HiGHS's highs_c_api.h,
-- but I do not know how to import these constants from a header
-- via e.g. foreign import
c_highs_status_error, c_highs_status_ok, c_highs_status_warning :: Status
c_highs_status_error   = -1
c_highs_status_ok      = 0 
c_highs_status_warning = 1

-------------------------------------------------------------------------------
-- HiGHS Matrix Format

type MatrixFormat = CInt

-- | These constants are defined in HiGHS's highs_c_api.h,
-- but I do not know how to import these constants from a header
-- via e.g. foreign import
c_highs_matrix_format_colwise, c_highs_matrix_format_rowwise :: MatrixFormat
c_highs_matrix_format_colwise = 1
c_highs_matrix_format_rowwise = 2

-------------------------------------------------------------------------------
-- HiGHS Objective Sense Direction

type ObjectiveSense = CInt

-- | These constants are defined in HiGHS's highs_c_api.h,
-- but I do not know how to import these constants from a header
-- via e.g. foreign import
c_highs_obj_sense_minimize, c_highs_obj_sense_maximize :: ObjectiveSense
c_highs_obj_sense_minimize = 1
c_highs_obj_sense_maximize = -1


-------------------------------------------------------------------------------
-- HiGHS Model Statuses

type ModelStatus = CInt

-- | These constants are defined in HiGHS's highs_c_api.h,
-- but I do not know how to import these constants from a header
-- via e.g. foreign import
c_highs_model_status_Notset, c_highs_model_status_LoadError, c_highs_model_status_ModelError, c_highs_model_status_PresolveError :: ModelStatus
c_highs_model_status_SolveError, c_highs_model_status_PostsolveError, c_highs_model_status_ModelEmpty, c_highs_model_status_Optimal :: ModelStatus
c_highs_model_status_Infeasible, c_highs_model_status_UnboundedOrInfeasible, c_highs_model_status_Unbounded, c_highs_model_status_ObjectiveBound :: ModelStatus
c_highs_model_status_ObjectiveTarget, c_highs_model_status_TimeLimit, c_highs_model_status_IterationLimit, c_highs_model_status_Unknown :: ModelStatus
c_highs_model_status_SolutionLimit, c_highs_model_status_Interrupt :: ModelStatus
c_highs_model_status_Notset = 0
c_highs_model_status_LoadError = 1
c_highs_model_status_ModelError = 2
c_highs_model_status_PresolveError = 3
c_highs_model_status_SolveError = 4
c_highs_model_status_PostsolveError = 5
c_highs_model_status_ModelEmpty = 6
c_highs_model_status_Optimal = 7
c_highs_model_status_Infeasible = 8
c_highs_model_status_UnboundedOrInfeasible = 9
c_highs_model_status_Unbounded = 10
c_highs_model_status_ObjectiveBound = 11
c_highs_model_status_ObjectiveTarget = 12
c_highs_model_status_TimeLimit = 13
c_highs_model_status_IterationLimit = 14
c_highs_model_status_Unknown = 15
c_highs_model_status_SolutionLimit = 16
c_highs_model_status_Interrupt = 17

-------------------------------------------------------------------------------
-- HiGHS Solving general linear programs

type NumberOfColumns = CInt
type NumberOfRows = CInt
type NumberNonZero = CInt
type ObjectiveConstantOffset = CDouble
type Array a = Ptr a

foreign import capi "interfaces/highs_c_api.h Highs_lpCall"
    c_highs_lpcall :: NumberOfColumns 
                   -> NumberOfRows 
                   -> NumberNonZero
                   -> MatrixFormat 
                   -> ObjectiveSense 
                   -> ObjectiveConstantOffset
                   -> Array CDouble -- ^ col_cost: An array of length [num_col] with the column costs.
                   -> Array CDouble -- ^ col_lower: An array of length [num_col] with the column lower bounds.
                   -> Array CDouble -- ^ col_upper: An array of length [num_col] with the column upper bounds.
                   -> Array CDouble -- ^ row_lower: An array of length [num_row] with the row lower bounds.
                   -> Array CDouble -- ^ row_upper: An array of length [num_row] with the row upper bounds.
                   -> Array CInt    -- ^ a_start:  The constraint matrix is provided to HiGHS in compressed sparse column form
                   -> Array CInt    -- ^ a_index: An array of length [num_nz] with indices of matrix entries.
                   -> Array CDouble -- ^ a_value: An array of length [num_nz] with values of matrix entries.
                   -> Array CDouble -- ^ col_value: An array of length [num_col], to be filled with the primal column solution.
                   -> Array CDouble -- ^ col_dual: An array of length [num_col], to be filled with the dual column solution.
                   -> Array CDouble -- ^ row_value: An array of length [num_row], to be filled with the primal row solution.
                   -> Array CDouble -- ^ row_dual: An array of length [num_row], to be filled with the dual row solution.
                   -> Ptr Status -- ^ col_basis_status: An array of length [num_col], to be filled with the basis status of the columns in the form of a kHighsBasisStatus constant.
                   -> Ptr Status -- ^ row_basis_status: An array of length [num_row], to be filled with the basis status of the rows in the form of a kHighsBasisStatus constant.
                   -> Ptr Status -- ^ model_status: The location in which to place the termination status of the model after the solve in the form of a c_highs_model_status_ constant.
                   -> IO Status

type HiGHSInstance = Ptr ()

foreign import capi "interfaces/highs_c_api.h Highs_create"  
    c_highs_create :: IO HiGHSInstance

foreign import capi "interfaces/highs_c_api.h Highs_destroy" 
    c_highs_destroy :: HiGHSInstance -> IO ()

foreign import capi "interfaces/highs_c_api.h Highs_passLp"  
    c_highs_passLp :: HiGHSInstance 
                   -> NumberOfColumns 
                   -> NumberOfRows 
                   -> NumberNonZero
                   -> MatrixFormat 
                   -> ObjectiveSense 
                   -> ObjectiveConstantOffset
                   -> Array CDouble -- ^ col_cost: An array of length [num_col] with the column costs.
                   -> Array CDouble -- ^ col_lower: An array of length [num_col] with the column lower bounds.
                   -> Array CDouble -- ^ col_upper: An array of length [num_col] with the column upper bounds.
                   -> Array CDouble -- ^ row_lower: An array of length [num_row] with the row lower bounds.
                   -> Array CDouble -- ^ row_upper: An array of length [num_row] with the row upper bounds.
                   -> Array CInt    -- ^ a_start:  The constraint matrix is provided to HiGHS in compressed sparse column form
                   -> Array CInt    -- ^ a_index: An array of length [num_nz] with indices of matrix entries.
                   -> Array CDouble -- ^ a_value: An array of length [num_nz] with values of matrix entries.
                   -> IO Status

foreign import capi "interfaces/highs_c_api.h Highs_setBoolOptionValue"
    c_highs_set_bool_option_value :: HiGHSInstance 
                                  -> CString 
                                  -> Bool 
                                  -> IO ()

foreign import capi "interfaces/highs_c_api.h Highs_run" 
    c_highs_run :: HiGHSInstance -> IO Status

foreign import capi "interfaces/highs_c_api.h Highs_getModelStatus" 
    c_highs_get_model_status :: HiGHSInstance -> IO ModelStatus

foreign import capi "interfaces/highs_c_api.h Highs_getSolution"
    c_highs_get_solution :: HiGHSInstance
                         -> Array CDouble -- ^ col_value: An array of length [num_col], to be filled with the primal column solution.
                         -> Array CDouble -- ^ col_dual: An array of length [num_col], to be filled with the dual column solution.
                         -> Array CDouble -- ^ row_value: An array of length [num_row], to be filled with the primal row solution.
                         -> Array CDouble -- ^ row_dual: An array of length [num_row], to be filled with the dual row solution.
                         -> IO ()
