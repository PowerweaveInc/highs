{-# LANGUAGE OverloadedLists #-}
module Test.Numeric.LinearProgramming ( tests ) where

import           Numeric.LinearProgramming
import           Test.Tasty            ( testGroup, TestTree )
import           Test.Tasty.HUnit      ( testCase, assertEqual )


tests :: TestTree
tests = testGroup "Numeric.LinearProgramming" 
      [ testMinimalAPIExample
      , testEqualityConstraint
      ]

testMinimalAPIExample :: TestTree
testMinimalAPIExample = testCase "Minimal API Example" $ do
    -- the following test case is a translation from Wikipedia's example
    -- here: https://en.wikipedia.org/wiki/Linear_programming#Example
    let objective = (4 *: "x1") +: (3 *: "x2")
        constraints = [ (1 *: "x1") +: (1 *: "x2") `boundedBy` (0 `to` 10)
                      , (3 *: "x1") +: (6 *: "x2") `boundedBy` (0 `to` 48)
                      , (4 *: "x1") +: (2 *: "x2") `boundedBy` (0 `to` 32)
                      ] 
        bounds = [ ("x1", from 0)
                 , ("x2", from 0) 
                 ]

        lp = linearProgram Maximize objective constraints bounds
        sol = solve lp
        expectation = MkSolution 36 [("x1", 6), ("x2", 4)]
    
    assertEqual mempty (Just expectation) sol


testEqualityConstraint :: TestTree
testEqualityConstraint = testCase "Equality constraint" $ do
    let objective = ((4 *: "x1") +: (3 *: "x2")) `offsetBy` 10
        constraints = [ (1 *: "x1") `eqTo` 1
                      , (2 *: "x2") `eqTo` 2
                      ] 
        bounds = [ ("x1", 0 `to` 10)
                 , ("x2", 0 `to` 10) 
                 ]

        lp = linearProgram Maximize objective constraints bounds
        sol = solve lp
        expectation = MkSolution (4 * 1 + 3 * 1 + 10) [("x1", 1), ("x2", 1)]
    
    assertEqual mempty (Just expectation) sol
