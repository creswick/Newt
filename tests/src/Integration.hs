module Integration where

import Control.Exception.Base (finally, catch, IOException )
import System.Directory ( removeFile, removeDirectory, getTemporaryDirectory
                        , copyFile )
import System.FilePath  ( (</>) )
import System.Process   ( rawSystem )
import System.Exit      ( ExitCode(..) )
import Data.UUID.V1 ( nextUUID )
import Data.UUID
import System.Unix.Directory ( withTemporaryDirectory )

import Prelude hiding (catch)

import Test.HUnit      ( (@=?), assertEqual, Assertion )
-- import Test.QuickCheck ( Property )
-- import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.Framework ( testGroup, Test )

import Newt.Utilities

tests :: [Test]
tests = [ testGroup "Simple File tests" [
                          testCase "Simple replacement test 1" $
                                   test_simpleReplace "The Author" "in.cabal.oracle.1"
                        , testCase "Simple replacement test 2" $
                                   test_simpleReplace "TheAuthor" "in.cabal.oracle.2"
                        ]
        , testGroup "Simple directory tests" [
                          testCase "Cabal project generation" $
                                   test_dirReplace projectTable "cabalProject" "cabalProjectOracle"
                        ]
        , testGroup "Inplace modification tests" [
                          testCase "Inplace replacement test 1" $
                                   test_inplaceReplace "The Author" "in.cabal.oracle.1"
                        , testCase "Inplace replacement test 2"  $
                                   test_inplaceReplace "TheAuthor" "in.cabal.oracle.2"
                        ]
        ]

projectTable :: [(String, String)]
projectTable = [ ("projName", "testProj")]

testDir :: FilePath
testDir = "tests" </> "testFiles"

newtCmd :: FilePath
newtCmd = "./cabal-dev/bin/newt"


test_inplaceReplace :: String -> String -> Assertion
test_inplaceReplace author oracleFile = do tmpFile <- getTmpFileName
                                           let input = (testDir </> "simpleTest" </> "in.cabal")
                                               source= "--source=" ++ tmpFile
                                               oracle = (testDir </> "simpleTest" </> oracleFile)
                                               params = [ "--inplace"
                                                        , "name=myProject"
                                                        , "author="++author]
                                           cleanup [tmpFile] $ do
                                             -- don't modify the original test input file:
                                             copyFile input tmpFile
                                             runNewt ([source] ++ params)
                                             -- check file content:
                                             assertFilesEqual "Generated file doesn't match" oracle tmpFile

test_simpleReplace :: String -> String -> Assertion
test_simpleReplace author oracleFile = do tmpFile <- getTmpFileName
                                          let source= "--source="++(testDir </> "simpleTest" </> "in.cabal")
                                              dest  = "--dest="++tmpFile
                                              oracle = (testDir </> "simpleTest" </> oracleFile)
                                              table = [ "name=myProject"
                                                      , "author=" ++ author ]
                                          cleanup [tmpFile] $ do
                                            runNewt (table ++ [source, dest])
                                            -- check file content:
                                            assertFilesEqual "Generated file doesn't match" oracle tmpFile

assertFilesEqual :: String -> FilePath -> FilePath -> Assertion
assertFilesEqual msg oracle suspect = do oracleTxt <- readFile oracle
                                         suspectTxt <- readFile suspect
                                         assertEqual msg oracleTxt suspectTxt

-- assertFilePathEqual :: String -> FilePath -> FilePath
-- assertFilePathEqual msg oracle suspect = 

assertDirsEqual :: String -> FilePath -> FilePath -> Assertion
assertDirsEqual msg oracle suspect = assertEqual msg False True

-- | Generates a filename with a uuid in either the system temp
-- directory or the current directory (if the system temp dir can't be
-- found).  Does not create the file, or verify uniqueness, just
-- generates the name.
getTmpFileName :: IO FilePath
getTmpFileName = do tempdir <- catch (getTemporaryDirectory) errHandler
                    Just uuid <- nextUUID -- XXX will fail if you don't have a MAC
                    return (tempdir </> toString uuid)
    where errHandler :: IOException -> IO FilePath
          errHandler _ = return "."

runNewt :: [String] -> IO ExitCode
runNewt params = do exitCode <- rawSystem newtCmd params
                    assertEqual "invocation of newt failed" ExitSuccess exitCode
                    return exitCode

test_dirReplace :: [(String, String)] -> FilePath -> FilePath -> Assertion
test_dirReplace table inDir oracle = withTemporaryDirectory "newt-XXXXXX" $ \dir -> do
                                       let outDir = dir </> oracle
                                           params = map (\(k,v)->k++"="++v) table
                                       runNewt (params ++ ["--source="++inDir, "--dest="++outDir])
                                       assertDirsEqual "Directory replacement failed." oracle outDir
