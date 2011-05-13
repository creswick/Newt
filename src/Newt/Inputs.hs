module Newt.Inputs where

import System.Directory ( doesDirectoryExist, doesFileExist )

import Control.Exception   ( catch, IOException )
import Control.Monad.Error ( ErrorT, throwError, liftIO )
import Data.Text.Encoding  ( decodeUtf8' )
import Data.Text           ( unpack )

import Prelude hiding (catch)

-- should be lazy?
import qualified Data.ByteString.Char8 as C8
import Data.ByteString ( ByteString )

data InputSpec = StandardIn
               | TxtFile FilePath
               | Directory FilePath
               | BinFile FilePath
                 deriving (Show)

inputSpec :: Maybe FilePath -> ErrorT String IO InputSpec
inputSpec Nothing    = return StandardIn
inputSpec (Just pth) = do dirExists  <- liftIO $ doesDirectoryExist pth
                          fileExists <- liftIO $ doesFileExist pth
                          case dirExists of
                            True  -> return (Directory pth)
                            False -> case fileExists of
                                       False -> throwError (pth++" Does not exist!")
                                       True  -> do isT <- liftIO $ isText pth
                                                   case isT of
                                                     True  -> return (TxtFile pth)
                                                     False -> return (BinFile pth)

isText :: FilePath -> IO Bool
isText path = do content <- C8.readFile path
                 return $ case decodeUtf8' content of
                            Left  _ -> False
                            Right _ -> True