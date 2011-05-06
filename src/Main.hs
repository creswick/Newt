module Main where

import Data.List  ( partition, elemIndex )
import Data.Maybe ( mapMaybe )
import qualified Data.Set as Set

import System.Environment ( getArgs )


import Newt.Newt

main :: IO ()
main = do args <- getArgs
          simpleTag <- mkSimpleTag "<<<" ">>>"
          let (rawPairs, files) = partition isPair args
              table             = mapMaybe strToPair rawPairs
              replacement input output = replaceFile simpleTag table input output

          case (length files) of
            2 -> replacement (head files) (files!!1)
            1 -> printTags simpleTag (head files)
            _ -> printHelp

printTags :: Tag a => a -> FilePath -> IO ()
printTags tag file = do tagSet <- getTags tag file
                        mapM_ putStrLn $ Set.toList tagSet

printHelp :: IO ()
printHelp = putStrLn "Usage: newt <inFile> [<outFile> [key=value]]"

isPair :: String -> Bool
isPair str = '=' `elem` str

strToPair :: String -> Maybe (String, String)
strToPair str = do idx <- elemIndex '=' str
                   let (key, rawValue) = splitAt idx str
                   return (key, tail rawValue)