module UtilityTests where

import Test.HUnit      ( (@=?) )
import Test.Framework.Providers.HUnit
import Test.Framework ( testGroup, Test )

import Newt.Utilities
import Newt.Inputs

import TestUtilities

tests :: [Test]
tests = [testGroup "trim tests" $ map (genTest trim) [
                         ("Empty string",      "",  "")
                       , ("leading space 1",   " a", "a")
                       , ("leading space 2",   "  a", "a")
                       , ("trailing space 1",  "a ", "a")
                       , ("trailing space 2",  "a  ", "a")
                       , ("interior space 1",  " a b ", "a b")
                       , ("mixed 1",           "   a  ", "a")
                       , ("mixed 2",           " \t  a  \n", "a")
                       , ("mixed 3",           " \t  a b \r\n", "a b")
                       ]
        , testGroup "File processing tests" $ map (genTestIO isText) [
                          ("cabal file", "newt.cabal", True)
                        , ("image", "tests/testFiles/sampleImage.png", False)
                        ]
        ]