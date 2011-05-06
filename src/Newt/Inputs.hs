module Newt.Inputs where

import System.Directory ( doesDirectoryExist, doesFileExist )

import Control.Monad.Error ( ErrorT, throwError, liftIO )

data InputKind = TxtFile FilePath
               | Directory FilePath
                 -- | TarGz FilePath
                 -- | Zip FilePath
                 deriving (Show)

inputKind :: FilePath -> ErrorT String IO InputKind
inputKind pth = do dirExists  <- liftIO $ doesDirectoryExist pth
                   fileExists <- liftIO $ doesFileExist pth
                   case dirExists of
                     True  -> return (Directory pth)
                     False -> case fileExists of
                                True  -> return (TxtFile pth)
                                False -> throwError (pth++" Does not exist!")

