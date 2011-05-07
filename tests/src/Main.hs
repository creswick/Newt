module Main where


-- import qualified MB.ParamTests as ParamTests
import qualified ReplaceTests as ReplaceTests

import Test.Framework ( defaultMain )

import Newt.Newt ( defaultPrefix, defaultSuffix, mkSimpleTag )

main :: IO ()
main = do tag <- mkSimpleTag (defaultPrefix, defaultSuffix)
          defaultMain [ ReplaceTests.tests tag
                      --                   , InitTests.tests
                      ]