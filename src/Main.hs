module Main where

import Data.List ( elemIndex, partition )
import Data.Array (elems)
import Data.Maybe ( mapMaybe )

import System.Environment ( getArgs )
import Text.Regex.PCRE ( Regex )
import Text.Regex.PCRE.String ( compile, compUngreedy, execBlank,
                                MatchOffset )
import Text.Regex.Base.RegexLike ( matchAllText )

class Tag a where
    tagRegex :: a -> IO Regex
    stripTag :: a -> (String -> String)

data TagSyntax = TagSyntax { start :: String
                           , end :: String
                           } deriving (Read, Show)

instance Tag TagSyntax where
    tagRegex (TagSyntax s e) = mkRegex s e
    stripTag (TagSyntax s e) str = reverse $ drop (length e) $ reverse $ drop (length s) str

simpleTag :: TagSyntax
simpleTag = TagSyntax "<<<" ">>>"

mkRegex :: String -> String -> IO Regex
mkRegex front back = do res <- makeRegex (front++".+"++back)
                        case res of
                          Left (offset, err) -> error err
                          Right regex        -> return regex

main :: IO ()
main = do args <- getArgs
          let (rawPairs, files) = partition isPair args
              table             = mapMaybe strToPair rawPairs
          replaceFile simpleTag table (head files) (files!!1)

isPair :: String -> Bool
isPair str = '=' `elem` str

strToPair :: String -> Maybe (String, String)
strToPair str = do idx <- elemIndex '=' str
                   let (key, rawValue) = splitAt idx str
                   return (key, tail rawValue)

replaceFile :: Tag a => a -> [(String, String)] -> FilePath -> FilePath -> IO ()
replaceFile tag table inFile outFile = do regex <- tagRegex tag
                                          content <- readFile inFile
                                          let result = regexReplace regex replaceFn content

                                          writeFile outFile result
    where stripTags = stripTag tag
          replaceFn str = case lookup (stripTags str) table of
                            Nothing -> str
                            Just s  -> s


makeRegex :: String -> IO (Either (MatchOffset, String) Regex)
makeRegex str = compile compUngreedy execBlank str

regexReplace :: Regex -> (String -> String) -> String -> String
regexReplace regexp fn input =
    -- matches :: [(String, (Int, Int))]
    let matches = map (head . elems) (matchAllText regexp input)
    in case matches of
         [] -> input
         _  -> let (prefix, offset) = foldl (builder input fn) ("", 0) matches
               in prefix ++ (drop offset input)

builder :: String -> (String -> String) -> (String, Int) -> (String, (Int, Int)) -> (String, Int)
builder input fn (acc, loc) (str, (offset, len))  =
    let filler = take (offset - loc) (drop loc input)
        newLoc = offset + len
    in (acc ++ filler ++ fn str, newLoc)
