
module Test.Numeric.LinearProgramming.Program (tests) where

import qualified Data.Vector.Unboxed  as Unboxed
import           Hedgehog             ( (===), property, forAll )
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Numeric.LinearProgramming ( fromCoefficients, coefficients )

import           Test.Tasty            ( testGroup, TestTree )
import           Test.Tasty.Hedgehog   ( testProperty )
import           Test.Tasty.HUnit      ( testCase, assertEqual )

tests :: TestTree
tests = testGroup "Numeric.LinearProgramming.Program" 
      [ testFromCoefficientsDuplicates
      , testLinearFunctionSemigroup
      ]


testFromCoefficientsDuplicates :: TestTree
testFromCoefficientsDuplicates 
    = testGroup "fromCoefficients in the presence of duplicates" 
    [ testCase "simple case" $
            assertEqual mempty (fromCoefficients [('a', 3::Int), ('b', 5)]) (fromCoefficients [('a', 1), ('a', 2), ('b', 5)])
    , testProperty "duplicate variables are added" $ property $ do
        vars <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
        (coeffs :: [Int]) <- forAll $ Gen.list (Range.singleton (length vars)) (Gen.integral $ Range.linear (-10) 10)
        sum coeffs === (Unboxed.sum $ fst $ coefficients $ fromCoefficients $ zip vars coeffs)
    ]


testLinearFunctionSemigroup :: TestTree
testLinearFunctionSemigroup = testProperty "the sum of coefficients is conserved by (<>)" $ property $ do
    vars1 <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
    (coeffs1 :: [Int]) <- forAll $ Gen.list (Range.singleton (length vars1)) (Gen.integral $ Range.linear (-10) 10)

    vars2 <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
    (coeffs2 :: [Int]) <- forAll $ Gen.list (Range.singleton (length vars2)) (Gen.integral $ Range.linear (-10) 10)

    let linfunc1 = fromCoefficients $ zip vars1 coeffs1
        linfunc2 = fromCoefficients $ zip vars2 coeffs2
    
    (sum (coeffs1 <> coeffs2)) === Unboxed.sum (fst $ coefficients $ linfunc1 <> linfunc2)