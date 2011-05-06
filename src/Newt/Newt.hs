module Newt.Newt where

import Data.Array ( elems )
import Data.Set   ( Set )
import qualified Data.Set as Set

import Text.Regex.PCRE ( Regex )
import Text.Regex.PCRE.String ( compile, compUngreedy, execBlank,
                                MatchOffset )
import Text.Regex.Base.RegexLike ( matchAllText )


class Tag a where
    tagRegex :: a -> Regex
    stripTag :: a -> (String -> String)

data TagSyntax = TagSyntax { tagStart :: String
                           , tagEnd :: String
                           , builtRegex :: Regex
                           }

instance Tag TagSyntax where
    tagRegex              tag      = builtRegex tag
    stripTag (TagSyntax s e _) str = reverse $ drop (length e) $ reverse $ drop (length s) str

mkSimpleTag :: String -> String -> IO TagSyntax
mkSimpleTag front back = do regex <- mkRegex front back
                            return $ TagSyntax front back regex

getTags :: Tag a => a -> FilePath -> IO (Set String)
getTags tag file = do content <- readFile file
                      let regexp = tagRegex tag
                          matches = map (head . elems) (matchAllText regexp content)
                          toStr (s, _) = (stripTag tag) s
                      return $ Set.fromList $ map toStr matches

replaceFile :: Tag a => a -> [(String, String)] -> FilePath -> FilePath -> IO ()
replaceFile tag table inFile outFile = do content <- readFile inFile
                                          let result = populate tag table content
                                          writeFile outFile result

populate :: Tag a => a -> [(String, String)] -> String -> String
populate tag table template = regexReplace (tagRegex tag) replaceFn template
    where stripTags     = stripTag tag
          replaceFn str = case lookup (stripTags str) table of
                            Nothing -> str
                            Just s  -> s

makeRegex :: String -> IO (Either (MatchOffset, String) Regex)
makeRegex str = compile compUngreedy execBlank str

mkRegex :: String -> String -> IO Regex
mkRegex front back = do res <- makeRegex (front++".+"++back)
                        case res of
                          Left ( _ , err) -> error err
                          Right regex     -> return regex

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
