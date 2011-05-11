module Newt.Newt where

import Control.Exception.Base ( IOException )
import Control.Monad       ( zipWithM )
import Control.Monad.Error ( ErrorT, runErrorT, liftIO )
import Data.Foldable ( foldrM )
import Data.Set   ( Set )
import qualified Data.Set as Set

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
    findTagStart :: a -> String -> Maybe (ShowS, String)
    findTagEnd   :: a -> String -> Maybe (ShowS, String)
    mkSubstKey   :: a -> String -> String

data TagSyntax = TagSyntax { tagStart :: String
                           , tagEnd :: String
                           }

findTok :: String -> String -> Maybe (ShowS, String)
findTok t = go id
    where
      go _   ""        = Nothing
      go acc s@(c:cs)  = case dropPrefix t s of
                           Nothing -> go (acc . (c:)) cs
                           Just s' -> Just (acc, s')

dropPrefix :: String -> String -> Maybe String
dropPrefix pfx s = go pfx s
    where
      go (c1:cs1) (c2:cs2) | c1 == c2  = go cs1 cs2
      go []       s2                   = Just s2
      go _        _                    = Nothing

instance Tag TagSyntax where
    findTagStart   = findTok . tagStart
    findTagEnd     = findTok . tagEnd
    mkSubstKey t s = tagStart t ++ s ++ tagEnd t

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
mkSimpleTag :: (String, String) -> TagSyntax
mkSimpleTag (front, back) = TagSyntax front back

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

findNextTag :: Tag a => a -> String -> Maybe (ShowS, String, String)
findNextTag t s = do
  (before, atTagStart) <- findTagStart t s
  (substKeyS, atTagEnd) <- findTagEnd t atTagStart
  return (before, substKeyS "", atTagEnd)

-- |Replace tags in the input string
populate :: Tag a => a -> Replace -> String -> String
populate t subst s = go s ""
  where
    go here =
        case findNextTag t here of
          Nothing                           -> showString here
          Just (before, substKey, atTagEnd) ->
              let substitution = case subst substKey of
                                   Nothing        -> mkSubstKey t substKey
                                   Just newValue  -> newValue
              in before . showString substitution . go atTagEnd

-- | Retrieve the set of @key@s found in a given string.
-- empty strings are not valid.
getTags :: Tag a => a -> String -> Set String
getTags tag str = Set.filter (/="") $ go Set.empty str
    where
      go found s =
          case findNextTag tag s of
            Nothing -> found
            Just (_, k, rest) -> go (Set.insert k found) rest
