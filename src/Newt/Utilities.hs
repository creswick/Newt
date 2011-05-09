module Newt.Utilities
    ( copyDirectory
    , cleanup
    ) where

import Prelude hiding    ( catch )
import Control.Exception ( IOException, catch, finally )
import System.Directory ( removeFile, removeDirectory )
import System.Process ( rawSystem )

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