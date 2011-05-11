module ReplaceTests where

import qualified Data.Set as Set

import Test.HUnit      ( (@=?) )
import Test.QuickCheck ( Property )
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.Framework ( testGroup, Test )

import Newt.Newt ( populate, Replace, Table, replaceTable, Tag
                 , defaultPrefix, defaultSuffix, mkSimpleTag
                 , getTags)

tests :: IO [Test]
tests = do let defaultTag = mkSimpleTag (defaultPrefix, defaultSuffix)
               dashTag    = mkSimpleTag ("---", "---")
           return $  [ testGroup "populate" $
                                 concat [map (testPopulate defaultTag $ replaceTable replacements)
                                         [ ("Empty string", "", "")
                                         , ("Empty tag", "<<<>>>", "<<<>>>")
                                         , ("Empty tag", "<<<k>>>", "v")
                                         , ("Nested tag", "<<<<<<k>>>>>>", "<<<<<<k>>>>>>")
                                         , ("Simple confusing context", "<<<<k>>>", "<<<<k>>>")
                                         , ("Unknown key", "<<<BadKey>>>", "<<<BadKey>>>")
                                         , ("key uses prefix", "<<<key<<<key>>>", "ok")
                                         , ("Multi-tag 1", "<<<k>>> <<<k>>>", "v v")
                                         , ("Multi-tag 2", "<<<k>>> <<<key>>>", "v value")
                                         , ("value uses tag markers", "<<<anotherkey>>>", "<<<newKey>>>")
                                         ]
                                        , [ testGroup "Random surrounding string"
                                            [ testProperty "default tag" $
                                                           prop_populateKeyInRandStr defaultTag defaultPrefix defaultSuffix replacements
                                            , testProperty "dash tag"    $
                                                           prop_populateKeyInRandStr dashTag "---" "---" replacements
                                            ]
                                          ]
                                        ]
                      , testGroup "getTags" $
                                  concat [ map (testGetTags defaultTag)
                                           [ ("Empty string", "", [])
                                           , ("Empty tag", "<<<>>>", [])
                                           , ("Empty tag", "<<<k>>>", ["k"])
                                           , ("Nested tag", "<<<<<<k>>>>>>", ["<<<k"])
                                           , ("Simple confusing context", "<<<<k>>>", ["<k"])
                                           , ("key uses prefix", "<<<key<<<key>>>", ["key<<<key"])
                                           , ("Multi-tag 1", "<<<k>>> <<<k>>>", ["k"])
                                           , ("Multi-tag 2", "<<<k>>> <<<key>>>", ["k", "key"])
                                           , ("Multi-tag 3", "<<<k>>><<<key>>> <<<k>>>", ["k", "key"])
                                           ]
                                         , [ testProperty "random content, default tag" $
                                                          prop_getTags defaultTag defaultPrefix defaultSuffix
                                           , testProperty "random content, dash tag" $
                                                          prop_getTags dashTag "---" "---"
                                           ]
                                         ]
                      ]


-- | Generate random strings, slap a key between them, and see if newt
-- can do the replacement:
prop_populateKeyInRandStr :: Tag a => a -> String -> String -> Table -> String -> String -> Bool
prop_populateKeyInRandStr tag tPrefix tSuffix table pfx sfx =
    let key   = (fst . head) table
        value = (snd . head) table
        input = pfx ++ tPrefix ++ key ++ tSuffix ++ sfx
        oracle = pfx ++ value ++ sfx
    in populate tag (replaceTable table) input == oracle

prop_getTags :: Tag a => a -> String -> String -> [(String, String)] -> String -> Bool
prop_getTags tag tPrefix tSuffix keys end =
    let input  = foldr (\(filler, key) front -> front ++ filler ++ tPrefix ++ key ++ tSuffix) end keys
        oracle = Set.fromList $ map snd keys
    in getTags tag input == oracle

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

testGetTags :: Tag a => a -> (String, String, [String]) -> Test
testGetTags tag (descr, input, oracle) =
    testCase (descr++" input: "++show input) assert
        where assert = Set.fromList oracle @=? getTags tag input
