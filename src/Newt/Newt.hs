module Newt.Newt where

import Control.Exception.Base ( IOException )
import Data.Array ( elems )
import Data.Foldable ( foldrM )
import Data.Set   ( Set )
import qualified Data.Set as Set
import Data.Maybe ( fromMaybe )

import Text.Regex.PCRE ( Regex )
import Text.Regex.PCRE.String ( compile, compUngreedy, execBlank,
                                MatchOffset )
import Text.Regex.Base.RegexLike ( matchAllText )
import System.Directory ( doesDirectoryExist )
import System.FilePath.Find ( findWithHandler, always )
import System.IO ( hGetContents, stdin )

import Newt.Inputs
import qualified Newt.Inputs as In
import Newt.Outputs
import qualified Newt.Outputs as Out


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

mkSimpleTag :: (String, String) -> IO TagSyntax
mkSimpleTag (front, back) = do regex <- mkRegex front back
                               return $ TagSyntax front back regex

getTagsFile :: Tag a => a -> FilePath -> IO (Set String)
getTagsFile tag file = do content <- readFile file
                          return $ getTags tag content

-- filesIn :: FilePath -> IO [FilePath]
-- filesIn dir = findWithHandler onErr always always dir
--  where onErr :: FilePath -> IOException -> IO [FilePath]
--        onErr file e = do
--          putStrLn ("Error folding over files on: "++file++"\n error:"++show e)
--          return [file]


-- | Collect the key names for every tag in the contents of the
-- specified directory.  This does currently return tags in the
-- directory name itself.  That could be confusing, but I think it's a
-- corner case.
--
-- XXX: Does not check to see if dir is actually a directory.
getTagsDirectory :: Tag a => a -> FilePath -> IO (Set String)
getTagsDirectory tag dir = do fileList <- findWithHandler onErr always always dir
                              foldrM acc Set.empty fileList

--foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b

 where onErr :: FilePath -> IOException -> IO [FilePath]
       onErr file e = do
         putStrLn ("Error folding over files on: "++file++"\n error:"++show e)
         return [file]

       acc :: FilePath -> Set String -> IO (Set String)
       acc file set = do cTags <- contentTags file
                         return $ Set.unions [ set
                                             , getTags tag file -- get the tags from the file name.
                                             , cTags -- the content tags.
                                             ]

       contentTags :: FilePath -> IO (Set String)
       contentTags file = do dirExists <- doesDirectoryExist file
                             case dirExists of
                               True  -> return $ Set.empty -- the recursive case is covered by findWithHandler.
                               False -> getTagsFile tag file

getTags :: Tag a => a -> String -> Set String
getTags tag content = let regexp       = tagRegex tag
                          matches      = map (head . elems) (matchAllText regexp content)
                          toStr (s, _) = (stripTag tag) s
                      in  Set.fromList $ map toStr matches

-- |Compute a replacement for a token
type Replace = String -> Maybe String

-- |A table of replacements
type Table = [(String, String)]

-- |Compute a template replacement by looking up in a table
replaceTable :: Table -> Replace
replaceTable t = \k -> lookup k t

replaceFile :: Tag a => a -> Replace -> InputSpec -> OutputSpec -> IO ()
replaceFile tag replace StandardIn          outSpec = do content <- hGetContents stdin
                                                         let result = populate tag replace content
                                                         writeTo outSpec result
replaceFile tag replace (In.TxtFile inFile) outSpec = do content <- readFile inFile
                                                         let result = populate tag replace content
                                                         writeTo outSpec result
replaceFile tag replace (In.Directory inDir) (Out.Directory outDir) = undefined
replaceFile _ _ _ _ = putStrLn "Unsupported input/output pairing"

populate :: Tag a => a -> Replace -> String -> String
populate tag replace template = regexReplace (tagRegex tag) replaceFn template
    where stripTags     = stripTag tag
          replaceFn str = fromMaybe str $ replace $ stripTags str

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
