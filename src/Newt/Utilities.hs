module Newt.Utilities
    ( copyDirectory
    )where

-- | Currently using rawSystem
--
-- XXX this doesn't do proper exception handling, at all.  Sorry about that.
copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory dir newLocation = rawSystem "cp" ["-rf", dir, newLocation]