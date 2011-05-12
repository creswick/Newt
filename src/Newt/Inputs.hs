module Newt.Inputs where

import System.Directory ( doesDirectoryExist, doesFileExist )

import Control.Monad.Error ( ErrorT, throwError, liftIO )

data InputSpec = StandardIn
               | TxtFile FilePath
               | Directory FilePath
                 deriving (Show)

inputSpec :: Maybe FilePath -> ErrorT String IO InputSpec
inputSpec Nothing    = return StandardIn
inputSpec (Just pth) = do dirExists  <- liftIO $ doesDirectoryExist pth
                          fileExists <- liftIO $ doesFileExist pth
                          case dirExists of
                            True  -> return (Directory pth)
                            False -> case fileExists of
                                       True  -> return (TxtFile pth)
                                       False -> throwError (pth++" Does not exist!")
