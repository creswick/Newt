{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad.Error (runErrorT)
import Data.List  ( partition, elemIndex )
import Data.Maybe ( mapMaybe )
import Data.Version ( showVersion )
import qualified Data.Set as Set

import System.Console.CmdArgs.Implicit
import System.Environment ( getArgs )

import Paths_newt ( version )

import Newt.Newt
import Newt.Inputs

data Config = Config { source :: Maybe FilePath
                     , dest   :: Maybe FilePath
                     , table  :: [String]
                     , list   :: Bool
                     } deriving (Show, Data, Typeable)

config = Config { source = def &= help "Template source location"
                , dest   = def &= help "Destination location"
                , table  = def &= args --  &= help "The list of \"key=value\" pairs to use."
                , list   = def &= help "List the set of keys in the input template."
                } &= summary versionString

versionString :: String
versionString = "newt " ++ showVersion version

main :: IO ()
main = do -- conf <- cmdArgs versionString [config]
          args <- getArgs
          simpleTag <- mkSimpleTag "<<<" ">>>"
          let (rawPairs, files) = partition isPair args
              table             = mapMaybe strToPair rawPairs
              replacement input output = replaceFile simpleTag table input output

          case (length files) of
            2 -> do eInSpec <- runErrorT (inputSpec $ head files)
                    case eInSpec of
                      Left err     -> putStrLn err
                      Right inSpec -> replacement inSpec (files!!1)
            1 -> do eInSpec <- runErrorT (inputSpec $ head files)
                    case eInSpec of
                      Left err     -> putStrLn err
                      Right inSpec -> printTags simpleTag inSpec
            _ -> printHelp

printTags :: Tag a => a -> InputSpec -> IO ()
printTags tag (TxtFile file)  = do tagSet <- getTagsFile tag file
                                   mapM_ putStrLn $ Set.toList tagSet
printTags tag (Directory pth) = do tagSet <- getTagsDirectory tag pth
                                   mapM_ putStrLn $ Set.toList tagSet
printTags _ _ = putStrLn "unsupported input format"


printHelp :: IO ()
printHelp = putStrLn "Usage: newt <inFile> [<outFile> [key=value]]"

isPair :: String -> Bool
isPair str = '=' `elem` str

strToPair :: String -> Maybe (String, String)
strToPair str = do idx <- elemIndex '=' str
                   let (key, rawValue) = splitAt idx str
                   return (key, tail rawValue)