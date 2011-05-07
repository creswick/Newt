module ReplaceTests where

import Test.HUnit      ( (@=?) )
import Test.QuickCheck ( Property )
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.Framework ( testGroup, Test )

import Newt.Newt ( populate, Replace, Table, replaceTable, Tag
                 , defaultPrefix, defaultSuffix)

tests :: Tag a => a -> Test
tests tag = testGroup "Content replacement tests" [
             testGroup "populate" $
                       concat [map (testPopulate tag $ replaceTable replacements)
                               [ ("Empty string", "", "")
                               , ("Empty tag", "<<<>>>", "<<<>>>")
                               , ("Empty tag", "<<<k>>>", "v")
                               , ("Nested tag", "<<<<<<k>>>>>>", "<<<v>>>")
                               , ("Unknown key", "<<<BadKey>>>", "<<<BadKey>>>")
                               , ("key uses prefix", "<<<key<<<key>>>", "ok")
                               , ("Multi-tag 1", "<<<k>>> <<<k>>>", "v v")
                               , ("Multi-tag 2", "<<<k>>> <<<key>>>", "v value")
                               , ("value uses tag markers", "<<<anotherkey>>>", "<<<newKey>>>")
                               ]
                              , [ testProperty "Random surrounding string" $ prop_populateKeyInRandStr tag replacements
                               ]
                              ]
            ]

-- | Generate random strings, slap a key between them, and see if newt
-- can do the replacement:
prop_populateKeyInRandStr :: Tag a => a -> Table -> String -> String -> Bool
prop_populateKeyInRandStr tag table pfx sfx = let key   = (fst . head) table
                                                  value = (snd . head) table
                                                  input = pfx ++ defaultPrefix ++ key ++ defaultSuffix ++ sfx
                                                  oracle = pfx ++ value ++ sfx
                                              in populate tag (replaceTable table) input == oracle

replacements :: Table
replacements = [ ( "k", "v")
               , ( "key", "value")
               , ( "key<<<key", "ok")
               , ( "anotherkey", "<<<newKey>>>")
               ]

testPopulate :: Tag a => a -> Replace -> (String, String, String) -> Test
testPopulate tag fn (descr, input, oracle) =
    testCase (descr++" input: "++show input) assert
        where assert = oracle @=? populate tag fn input