module UtilityTests where

import Test.HUnit      ( (@=?) )
import Test.Framework.Providers.HUnit
import Test.Framework ( testGroup, Test )

import Newt.Utilities


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
        ]

genTest :: (Show a, Eq a) => (String -> a) -> (String, String, a) -> Test
genTest fn (descr, input, oracle) =
    testCase (descr++" input: "++show input) assert
        where assert = oracle @=? fn input