module ReplaceTests where

import qualified Data.Set as Set

import Safe

import Test.HUnit      ( (@=?) )
import Test.QuickCheck ( Property, (==>) )
import Test.QuickCheck.Property ()
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.Framework ( testGroup, Test )

import Newt.Newt ( populate, Replace, Table, replaceTable, Tag
                 , defaultPrefix, defaultSuffix, mkSimpleTag
                 , getTags, getTagsDirectory )

import TestUtilities

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
                                         -- , [ testProperty "random content, default tag" $
                                         --                  prop_getTags defaultTag defaultPrefix defaultSuffix
                                         --   , testProperty "random content, dash tag" $
                                         --                  prop_getTags dashTag "---" "---"
                                         --   ]
                                         ]
                     , testGroup "getTagsDirectory" $ map (testGetTagsDirectory defaultTag) [
                                       ("Cabal Project", "tests/testFiles/dirTemplates/cabalProject",
                                        ["author", "authoremail", "description", "projName", "synopsis", "year"])
                                     , ("Sample Image Proj", "tests/testFiles/dirTemplates/templateWithImages",
                                        ["name", "inner"])
                                     ]
                      ]


-- | Generate random strings, slap a key between them, and see if newt
-- can do the replacement:
prop_populateKeyInRandStr :: Tag a => a -> String -> String -> Table -> String -> Char -> String -> Char -> Property
prop_populateKeyInRandStr tag tPrefix tSuffix table pfx pCh sfx sCh = (Just pCh) /= headMay tPrefix &&
                                                                      (Just sCh) /= lastMay tSuffix ==>
    let key   = (fst . head) table
        value = (snd . head) table
        input = pfx ++ [pCh] ++ tPrefix ++ key ++ tSuffix ++ [sCh] ++ sfx
        oracle = pfx ++ [pCh] ++ value ++ [sCh] ++ sfx
    in populate tag (replaceTable table) input == oracle

emptyKorV :: (String, String) -> Bool
emptyKorV ("", _ ) = True
emptyKorV (_ , "") = True
emptyKorV (_ , _ ) = False

prop_getTags :: Tag a => a -> String -> String -> [(String, String)] -> String -> Property
prop_getTags tag tPrefix tSuffix keys end = filter (not . emptyKorV) keys /= [] ==>
    let filteredTable = filter (not . emptyKorV) keys
        input  = foldr (\(filler, key) front -> front ++ filler ++ tPrefix ++ key ++ tSuffix) end filteredTable
        oracle = Set.fromList $ map snd filteredTable
    in getTags tag input == oracle

replacements :: Table
replacements = [ ( "k", "v")
               , ( "key", "value")
               , ( "key<<<key", "ok")
               , ( "anotherkey", "<<<newKey>>>")
               ]

testPopulate :: Tag a => a -> Replace -> (String, String, String) -> Test
testPopulate tag fn = genTest (populate tag fn)

testGetTags :: Tag a => a -> (String, String, [String]) -> Test
testGetTags tag (descr, input, oracle) =
    testCase (descr++" input: "++show input) assert
        where assert = Set.fromList oracle @=? getTags tag input

testGetTagsDirectory :: Tag a => a -> (String, String, [String]) -> Test
testGetTagsDirectory tag (descr, input, oracle) =
    testCase (descr++" input: "++show input) assert
        where assert = do tags <- getTagsDirectory tag input
                          Set.fromList oracle @=? tags
