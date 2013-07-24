module Main where


import qualified Integration as Integration
import qualified ReplaceTests as ReplaceTests
import qualified UtilityTests as UtilityTests

import Test.Framework ( defaultMain )

main :: IO ()
main = do replaceTests <- ReplaceTests.tests
          defaultMain $ concat [ UtilityTests.tests
                               , replaceTests
                               , Integration.tests ]