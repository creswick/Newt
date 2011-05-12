module UtilityTests where

import Test.HUnit      ( (@=?) )
import Test.Framework.Providers.HUnit
import Test.Framework ( testGroup, Test )

import Newt.Utilities
import Newt.Inputs

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

genTest :: (Show a, Show b, Eq b) => (a -> b) -> (String, a, b) -> Test
genTest fn (descr, input, oracle) =
    testCase (descr++" input: "++show input) assert
        where assert = oracle @=? fn input

genTestIO :: (Show a, Show b, Eq b) => (a -> IO b) -> (String, a, b) -> Test
genTestIO fn (descr, input, oracle) = testCase (descr++" input: " ++show input) $ do
                                        res <- fn input
                                        oracle @=? res