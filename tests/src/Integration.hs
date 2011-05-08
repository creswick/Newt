module Integration where

import Control.Exception.Base (finally, catch, IOException )
import System.Directory ( removeFile, removeDirectory, getTemporaryDirectory
                        , copyFile )
import System.FilePath  ( (</>) )
import System.Process   ( rawSystem )
import System.Exit      ( ExitCode(..) )
import Data.UUID.V1 ( nextUUID )
import Data.UUID

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
        , testGroup "Inplace modification tests" [
                          testCase "Inplace replacement test 1" $
                                   test_inplaceReplace "The Author" "in.cabal.oracle.1"
                        , testCase "Inplace replacement test 2"  $
                                   test_inplaceReplace "TheAuthor" "in.cabal.oracle.2"
                        ]
        ]

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
                                             exitCode <- rawSystem newtCmd ([source] ++ params)
                                             assertEqual "invocation of newt failed" ExitSuccess exitCode
                                             -- check file content:
                                             assertFilesEqual "Generated file doesn't match" oracle tmpFile

test_simpleReplace :: String -> String -> Assertion
test_simpleReplace author oracleFile = do tmpFile <- getTmpFileName
                                          let source= "--source=" ++ (testDir </> "simpleTest" </> "in.cabal")
                                              dest  = "--dest="++tmpFile
                                              oracle = (testDir </> "simpleTest" </> oracleFile)
                                              params = [ "name=myProject"
                                                       , "author="++author]
                                          cleanup [tmpFile] $ do
                                            exitCode <- rawSystem newtCmd ([source, dest] ++ params)
                                            assertEqual "invocation of newt failed" ExitSuccess exitCode
                                            -- check file content:
                                            assertFilesEqual "Generated file doesn't match" oracle tmpFile

assertFilesEqual :: String -> FilePath -> FilePath -> Assertion
assertFilesEqual msg oracle suspect = do oracleTxt <- readFile oracle
                                         suspectTxt <- readFile suspect
                                         assertEqual msg oracleTxt suspectTxt

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

-- {- This function takes two parameters: a filename pattern and another
--    function.  It will create a temporary file, and pass the name and Handle
--    of that file to the given function.

--    The temporary file is created with openTempFile.  The directory is the one
--    indicated by getTemporaryDirectory, or, if the system has no notion of
--    a temporary directory, "." is used.  The given pattern is passed to
--    openTempFile.

--    After the given function terminates, even if it terminates due to an
--    exception, the Handle is closed and the file is deleted. -}
-- withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
-- withTempFile pattern func =
--     do -- The library ref says that getTemporaryDirectory may raise on
--        -- exception on systems that have no notion of a temporary directory.
--        -- So, we run getTemporaryDirectory under catch.  catch takes
--        -- two functions: one to run, and a different one to run if the
--        -- first raised an exception.  If getTemporaryDirectory raised an
--        -- exception, just use "." (the current working directory).
--        tempdir <- catch (getTemporaryDirectory) (\_ -> return ".")
--        (tempfile, temph) <- openTempFile tempdir pattern

--        -- Call (func tempfile temph) to perform the action on the temporary
--        -- file.  finally takes two actions.  The first is the action to run.
--        -- The second is an action to run after the first, regardless of
--        -- whether the first action raised an exception.  This way, we ensure
--        -- the temporary file is always deleted.  The return value from finally
--        -- is the first action's return value.
--        finally (func tempfile temph) 
--                (do hClose temph
--                    removeFile tempfile)