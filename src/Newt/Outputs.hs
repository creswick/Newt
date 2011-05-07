module Newt.Outputs where

import System.Directory    ( doesDirectoryExist, doesFileExist )
import Control.Monad       ( when )
import Control.Monad.Error ( ErrorT, throwError, liftIO )


import qualified Newt.Inputs as In

data OutputSpec = StandardOut
                | TxtFile FilePath
                | Directory FilePath

outputSpec :: In.InputSpec -> Maybe FilePath -> ErrorT String IO OutputSpec
outputSpec (In.Directory _)  Nothing = throwError "Can not write directory input to standard output!"
outputSpec _                 Nothing = return StandardOut -- should check for compatability
outputSpec input (Just pth) = do dirExists  <- liftIO $ doesDirectoryExist pth
                                 fileExists <- liftIO $ doesFileExist pth
                                 when (dirExists || fileExists) (throwError (pth++" exists!"))

                                 case input of
                                  In.StandardIn  -> return $ TxtFile pth
                                  In.TxtFile   _ -> return $ TxtFile pth
                                  In.Directory _ -> return $ Directory pth