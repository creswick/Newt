module Newt.Utilities
    ( copyDirectory
    ) where

import System.Process ( rawSystem )

-- | Currently using rawSystem
--
-- XXX this doesn't do proper exception handling, at all.  Sorry about that.
copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory dir newLocation = do _ <- rawSystem "cp" ["-rf", dir, newLocation]
                                   return ()