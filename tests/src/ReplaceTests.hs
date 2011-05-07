module ReplaceTests where


import Test.Framework ( testGroup, Test )
import Test.Framework.Providers.HUnit
import Test.HUnit ( (@=?) )

import Newt.Newt ( populate, Replace, replaceTable, Tag )

tests :: Tag a => a -> Test
tests tag = testGroup "Content replacement tests" [
             testGroup "populate" $ map (testPopulate tag replacements) [
                             ("Empty string", "", "")
                           , ("Empty tag", "<<<>>>", "<<<>>>")
                           , ("Empty tag", "<<<k>>>", "v")
                           , ("Nested tag", "<<<<<<k>>>>>>", "<<<v>>>")
                           , ("Unknown key", "<<<BadKey>>>", "<<<BadKey>>>")
                           , ("key uses prefix", "<<<key<<<key>>>", "ok")
                           , ("Multi-tag 1", "<<<k>>> <<<k>>>", "v v")
                           , ("Multi-tag 2", "<<<k>>> <<<key>>>", "v value")
                           , ("value uses tag markers", "<<<anotherkey>>>", "<<<newKey>>>")
                           ]
            ]

replacements :: Replace
replacements = replaceTable [ ( "k", "v")
                            , ( "key", "value")
                            , ( "key<<<key", "ok")
                            , ( "anotherkey", "<<<newKey>>>")
                            ]

testPopulate :: Tag a => a -> Replace -> (String, String, String) -> Test
testPopulate tag fn (descr, input, oracle) =
    testCase (descr++" input: "++show input) assert
        where assert = oracle @=? populate tag fn input