module Newt.Outputs where

import System.Directory    ( doesDirectoryExist, doesFileExist
                           , copyFile )
import System.FilePath  ( (</>) )
import System.Unix.Directory ( withTemporaryDirectory )
import Control.Monad       ( when )
import Control.Monad.Error ( ErrorT, throwError, liftIO )



import qualified Newt.Inputs as In

data OutputSpec = StandardOut
                | File FilePath -- ^ output doesn't distinguish between
                                -- binary and text files.
                | Directory FilePath
                deriving (Show)

outputSpec :: Bool -> In.InputSpec -> Maybe FilePath -> ErrorT String IO OutputSpec
outputSpec True   inSpec                 _ = fromInputSpec inSpec
outputSpec False (In.Directory _)  Nothing = throwError "Can not write directory input to standard output!"
outputSpec False _                 Nothing = return StandardOut -- should check for compatability
outputSpec False input (Just pth) = do dirExists  <- liftIO $ doesDirectoryExist pth
                                       fileExists <- liftIO $ doesFileExist pth
                                       when (dirExists || fileExists) (throwError (pth++" exists!"))
                                       case input of
                                         In.StandardIn  -> return $ File pth
                                         In.TxtFile   _ -> return $ File pth
                                         In.Directory _ -> return $ Directory pth
                                         In.BinFile   _ -> return $ File pth

-- | Convert an inputspec into an output spec that represents the same
-- source.  used for inplace modifications.
fromInputSpec :: In.InputSpec -> ErrorT String IO OutputSpec
fromInputSpec (In.TxtFile file)  = return $ File file
fromInputSpec (In.BinFile file)  = return $ File file
fromInputSpec (In.Directory dir) = return $ Directory dir
fromInputSpec In.StandardIn      = throwError "Can not modify stdin inplace."

writeTo :: OutputSpec -> String -> IO ()
writeTo (File outFile) str = withTemporaryDirectory "newt-XXXXXX" $ \dir -> do
                               let tempOut = dir </> "tempOut"
                               writeFile tempOut str
                               copyFile  tempOut outFile
writeTo StandardOut    str = putStr str
writeTo outSpec          _ = error ("Could not write to outspec: "++show outSpec)