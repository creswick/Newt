module Newt.Newt where

import Control.Exception.Base ( IOException )
import Control.Monad       ( zipWithM )
import Control.Monad.Error ( ErrorT, runErrorT, liftIO )
import Data.Array ( elems )
import Data.Foldable ( foldrM )
import Data.Set   ( Set )
import qualified Data.Set as Set
import Data.Maybe ( fromMaybe )

import Text.Regex.PCRE ( Regex )
import Text.Regex.PCRE.String ( compile, compUngreedy, execBlank,
                                MatchOffset )
import Text.Regex.Base.RegexLike ( matchAllText )
import System.Directory ( doesDirectoryExist, getDirectoryContents
                        , createDirectoryIfMissing )
import System.Exit           ( exitWith, ExitCode(..) )
import System.FilePath       ( (</>) )
import System.FilePath.Find  ( findWithHandler, always )
import System.FilePath.Posix ( makeRelative )
import System.IO ( hGetContents, stdin )



import Newt.Inputs
import qualified Newt.Inputs as In
import Newt.Outputs
import qualified Newt.Outputs as Out

-- | Tag is an abstraction layer over the ways in which a given @key@
-- could be located in a file.  Currently, this does require that the
-- syntax for marking keys is regular.
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

-- | The default tag prefix.
defaultPrefix :: String
defaultPrefix = "<<<"

-- | The default tag suffix.
defaultSuffix :: String
defaultSuffix = ">>>"

-- | Create a simple 'tag', the pair of syntactic markers that
-- indicate where in a body of text to stick a value.  For example,
-- the defaults tag is created with:
--
-- > mkSimpleTag ("<<<", ">>>")
--
-- which corresponds to tags of the form:
--
-- @<<<key>>>@
mkSimpleTag :: (String, String) -> IO TagSyntax
mkSimpleTag (front, back) = do regex <- mkRegex front back
                               return $ TagSyntax front back regex

    where mkRegex :: String -> String -> IO Regex
          mkRegex front back = do res <- makeRegex (front++".+"++back)
                                  case res of
                                    Left ( _ , err) -> error err
                                    Right regex     -> return regex


-- | Retrieves the set of @key@s found in a text file.
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
getTagsDirectory tag dir = do fileList <- findWithHandler onFileIOErr always always dir
                              foldrM acc Set.empty fileList
 where acc :: FilePath -> Set String -> IO (Set String)
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

onFileIOErr :: FilePath -> IOException -> IO [FilePath]
onFileIOErr file e = do
  putStrLn ("Error folding over files on: "++file++"\n error:"++show e)
  return [file]

-- | Retrieve the set of @key@s found in a given string.
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

-- | Replace all the defined keys in a template, writing the template
-- out to the specified destination.
--
-- `replaceFile` performs IO as necessary to read/write templates and
-- results.  It is also poorly named: it operates on any input/output
-- spec.
--
-- XXX: return value should probably be @ErrorT String IO ()@
replaceFile :: Tag a => a -> Replace -> InputSpec -> OutputSpec -> IO ()
replaceFile tag replace StandardIn          outSpec = do content <- hGetContents stdin
                                                         let result = populate tag replace content
                                                         writeTo outSpec result
replaceFile tag replace (In.TxtFile inFile) outSpec = do content <- readFile inFile
                                                         let result = populate tag replace content
                                                         writeTo outSpec result
replaceFile tag replace (In.Directory inDir) (Out.Directory outDir) = do
  -- create the incomming dir.  This hapens first so the initial
  -- invocation doesn't do string replacement on the dir name.
  createDirectoryIfMissing True outDir
  -- get all the directory entries:
  inPaths <- do tempPaths <- getDirectoryContents inDir
                return $ map (inDir </>) (filter (\x-> (x /= ".") && (x /= "..")) tempPaths)

  let outPaths  = map inToOut inPaths
      inToOut f = Just (outDir </>  -- prepend the output dir to the paths.
                          (populate tag replace  -- run substitutions on file names.
                             (makeRelative inDir f))) -- get the relative locations for the incoming template files/dirs

  res <- runErrorT $ do
           inList  <- mapM inputSpec (map Just inPaths)
           outList <- zipWithM (outputSpec False) inList outPaths
           liftIO $ zipWithM (replaceFile tag replace) inList outList
  -- TODO replaceFile should return ErrorT String IO (), so this won't be necessary:
  case res of
    Left err -> do putStrLn err
                   exitWith (ExitFailure 1)
    Right _  -> return ()



replaceFile _ _ _ _ = putStrLn "Unsupported input/output pairing"

-- | Replace all the defined keys in a string, returning the populated string.
populate :: Tag a => a -> Replace -> String -> String
populate tag replace template = regexReplace (tagRegex tag) replaceFn template
    where stripTags     = stripTag tag
          replaceFn str = fromMaybe str $ replace $ stripTags str

-- | Helper function for compiling a non-greedy regular expression.
-- Might be better written as a result of @ErrorT String IO Regex@ to
-- match the other error types in Newt.
makeRegex :: String -> IO (Either (MatchOffset, String) Regex)
makeRegex str = compile compUngreedy execBlank str

-- | Apply @fn@ to every matched instance of @regexp@ in @input@,
-- returning the result.
regexReplace :: Regex -> (String -> String) -> String -> String
regexReplace regexp fn input =
    -- matches :: [(String, (Int, Int))]
    let matches = map (head . elems) (matchAllText regexp input)
    in case matches of
         [] -> input
         _  -> let (prefix, offset) = foldl (builder input fn) ("", 0) matches
               in prefix ++ (drop offset input)

-- | Helper to assemble the replaced strings with the content between
-- regular expression matches.
builder :: String -> (String -> String) -> (String, Int) -> (String, (Int, Int)) -> (String, Int)
builder input fn (acc, loc) (str, (offset, len))  =
    let filler = take (offset - loc) (drop loc input)
        newLoc = offset + len
    in (acc ++ filler ++ fn str, newLoc)
