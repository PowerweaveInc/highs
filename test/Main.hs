module Main ( main ) where

import qualified Test.Numeric.LinearProgramming
import qualified Test.Numeric.LinearProgramming.HiGHS
import qualified Test.Numeric.LinearProgramming.HiGHS.LowLevel
import qualified Test.Numeric.LinearProgramming.Program

import           Test.Tasty

main :: IO ()
main = defaultMain
     $ testGroup "Test suite"
     [ Test.Numeric.LinearProgramming.tests
     , Test.Numeric.LinearProgramming.HiGHS.tests
     , Test.Numeric.LinearProgramming.HiGHS.LowLevel.tests
     , Test.Numeric.LinearProgramming.Program.tests
     ]