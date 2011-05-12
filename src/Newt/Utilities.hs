module Newt.Utilities
    ( copyDirectory
    , cleanup
    , trim
    , isValueArg
    ) where

import Prelude hiding    ( catch )
import Control.Exception ( IOException, catch, finally )

import Data.Char (isSpace)
import System.Directory ( removeFile, removeDirectory )
import System.Process ( rawSystem )


isValueArg :: String -> Bool
isValueArg str = '=' `elem` str


-- | Currently using rawSystem
--
-- XXX this doesn't do proper exception handling, at all.  Sorry about that.
copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory dir newLocation = do _ <- rawSystem "cp" ["-rf", dir, newLocation]
                                   return ()

cleanup :: [FilePath] -> IO a -> IO a
cleanup files operation = do
  finally operation $ do
    mapM_ rmIfExists files

    where rmIfExists :: FilePath -> IO ()
          rmIfExists file = catch (removeFile file) (fileFailedHandler file)

          fileFailedHandler :: FilePath -> IOException -> IO ()
          fileFailedHandler file _e = catch (removeDirectory file) dirFailedHandler

          dirFailedHandler :: IOException -> IO ()
          dirFailedHandler _e = return ()


-- | remove leading / trailing whitespace.
trim :: String -> String
trim = reverse . dropSpaces . reverse . dropSpaces
    where dropSpaces = dropWhile isSpace