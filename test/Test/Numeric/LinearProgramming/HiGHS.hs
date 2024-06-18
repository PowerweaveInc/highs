{-# LANGUAGE OverloadedLists #-}
module Test.Numeric.LinearProgramming.HiGHS ( tests ) where

import           Control.Monad         ( forM_ )
import           Numeric.LinearProgramming       ( Direction(..) )
import           Numeric.LinearProgramming.HiGHS ( HiGHSSolution(..), optimize, sparseRepresentation, )
import           Test.Tasty            ( testGroup, TestTree )
import           Test.Tasty.HUnit      ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Numeric.LinearProgramming.HiGHS"
      [ testFullAPIExample
      , testRepeatedSolving
      , testSparseRepresentation 
      ]


testFullAPIExample :: TestTree
testFullAPIExample = testCase "HiGHS Full API Example (high-level)" $ do
    -- The following is a close translation of
    -- https://github.com/ERGO-Code/HiGHS/blob/77583e20470b2ed5dad05a8eb04511034710ddd2/examples/call_highs_from_c.c#L10

    -- Recall that constraint matrices are specified in column-format
    let mat = [ [0, 1]
              , [1, 2]
              , [3, 2]
              ]
        expectation = Just $ MkHiGHSSolution 2.75 [0.5, 2.25]
        sol = optimize Minimize [1, 1] 0 [(-1e20, 7), (5, 15), (6, 1e20)] [(0, 4), (1, 1e20)] mat
    assertEqual mempty expectation sol


testRepeatedSolving :: TestTree
testRepeatedSolving = testCase "Repeated optimizations works as expected" $ do
    -- Using the 'simple' HiGHS interface was failing after the first run. This test
    -- ensures that repeated optimizations work as expected
    let mat = [ [0, 1]
              , [1, 2]
              , [3, 2]
              ]
        expectation = Just $ MkHiGHSSolution 2.75 [0.5, 2.25]
    
    forM_ (replicate 100 (0 :: Int)) $ \_ -> do
        let sol = optimize Minimize [1, 1] 0 [(-1e20, 7), (5, 15), (6, 1e20)] [(0, 4), (1, 1e20)] mat
        assertEqual mempty expectation sol


testSparseRepresentation :: TestTree
testSparseRepresentation = testCase "sparseRepresentation" $ do
    -- Recall that constraint matrices are specified in column-format
    let mat = [ [0, 1]
              , [1, 2]
              , [3, 2]
              ]
        (numNz, starts, indices, values) = sparseRepresentation mat
    
    assertEqual "num_nz"   5           numNz
    assertEqual "a_start"  [0,1,3]      starts 
    assertEqual "a_index"  [1,0,1,0,1] indices
    assertEqual "a_values" [1,1,2,3,2] values