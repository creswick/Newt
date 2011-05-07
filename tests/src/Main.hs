module Main where


import qualified Integration as Integration
import qualified ReplaceTests as ReplaceTests

import Test.Framework ( defaultMain )



main :: IO ()
main = do replaceTests <- ReplaceTests.tests
          defaultMain $ concat [ replaceTests
                               , Integration.tests ]