module Main where

import Control.Monad.Error (runErrorT)
import Data.List  ( partition, elemIndex )
import Data.Maybe ( mapMaybe )
import qualified Data.Set as Set

import System.Environment ( getArgs )

import Newt.Newt
import Newt.Inputs

main :: IO ()
main = do args <- getArgs
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