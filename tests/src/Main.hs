module Main where


-- import qualified MB.ParamTests as ParamTests
import qualified ReplaceTests as ReplaceTests

import Test.Framework ( defaultMain )



main :: IO ()
main = do replaceTests <- ReplaceTests.tests
          defaultMain [ replaceTests
                      ]