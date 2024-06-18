module Test.Numeric.LinearProgramming.HiGHS.LowLevel ( tests ) where

import           Control.Exception     ( bracket )
import           Control.Monad         ( forM_ )
import           Foreign.C.String      ( newCString )
import           Foreign.C.Types       ( CInt, CDouble )
import           Foreign.Marshal.Array ( newArray )
import           Foreign.Ptr           ( Ptr )
import qualified Numeric.LinearProgramming.HiGHS.LowLevel as LowLevel
import           Test.Tasty            ( testGroup, TestTree )
import           Test.Tasty.HUnit      ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Numeric.LinearProgramming.HiGHS.LowLevel" [ testFullAPIExample ]


testFullAPIExample :: TestTree
testFullAPIExample = testCase "HiGHS Full API Example (low-level)" $ do
    
    -- The following is a close translation of
    -- https://github.com/ERGO-Code/HiGHS/blob/77583e20470b2ed5dad05a8eb04511034710ddd2/examples/call_highs_from_c.c#L304
    let num_col = 2 :: CInt
        num_row = 3 :: CInt
        num_nz = 5
        sense = LowLevel.c_highs_obj_sense_minimize
        offset = 3

    (col_cost :: Ptr CDouble)  <- newArray [1.0, 1.0]
    (col_lower :: Ptr CDouble) <- newArray [0.0, 1.0]
    (col_upper :: Ptr CDouble) <- newArray [4.0, 1e30]

    (row_lower :: Ptr CDouble) <- newArray [-1e30, 5.0, 6.0]
    (row_upper :: Ptr CDouble) <- newArray [7.0, 15.0, 1e30]

    let a_format = LowLevel.c_highs_matrix_format_colwise

    (a_start :: Ptr CInt)    <- newArray [0, 2]
    (a_index :: Ptr CInt)    <- newArray [1, 2, 0, 1, 2]
    (a_value :: Ptr CDouble) <- newArray [1.0, 3.0, 1.0, 2.0, 2.0]
    
    bracket LowLevel.c_highs_create LowLevel.c_highs_destroy $ \highs -> do

        -- The following lines are required to minimize the amount of
        -- garbage that HiGHS outputs to standard output.
        forM_ ["output_flag", "log_to_console"] $ \flag -> do
            flagname <- newCString flag
            LowLevel.c_highs_set_bool_option_value highs flagname False
        
        pass_status <- LowLevel.c_highs_passLp highs num_col num_row num_nz a_format 
                                              sense offset col_cost col_lower col_upper row_lower row_upper
                                              a_start a_index a_value
        assertEqual "Pass status" LowLevel.c_highs_status_ok pass_status

        run_status <- LowLevel.c_highs_run highs
        assertEqual "Run status" LowLevel.c_highs_status_ok run_status

        modelStatus <- LowLevel.c_highs_get_model_status highs
        assertEqual "Model statys" LowLevel.c_highs_model_status_Optimal modelStatus