module TestUtilities where

import Test.HUnit      ( (@=?) )
import Test.Framework.Providers.HUnit
import Test.Framework ( testGroup, Test )

genTest :: (Show a, Show b, Eq b) => (a -> b) -> (String, a, b) -> Test
genTest fn (descr, input, oracle) =
    testCase (descr++" input: "++show input) assert
        where assert = oracle @=? fn input

genTestIO :: (Show a, Show b, Eq b) => (a -> IO b) -> (String, a, b) -> Test
genTestIO fn (descr, input, oracle) = testCase (descr++" input: " ++show input) $ do
                                        res <- fn input
                                        oracle @=? res