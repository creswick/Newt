{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad.Error (runErrorT, liftIO)
import Data.List  ( elemIndex )
import Data.Maybe ( mapMaybe, fromMaybe )
import Data.Version ( showVersion )
import qualified Data.Set as Set

import System.Console.CmdArgs.Implicit
import System.Environment ( getArgs, withArgs )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( hGetContents, stdin )

import Paths_newt ( version )

import Newt.Newt
import Newt.Inputs
import qualified Newt.Inputs as In
import Newt.Outputs
import qualified Newt.Outputs as Out

data Config = Config { source    :: Maybe FilePath
                     , dest      :: Maybe FilePath
                     , rawTable  :: [String]
                     , list      :: Bool
                     , inplace   :: Bool
                     , prefix    :: Maybe String
                     , suffix    :: Maybe String
                     } deriving (Show, Data, Typeable)

config :: Config
config = Config { source   = def &= name "s"
                           &= help ("Template source location.  Default is to"++
                                    " read from stdin.")
                , dest     = def
                           &= help ("Destination location.  Default is to"++
                                    " write to stdout, but not all inputs"++
                                    "can be written to stdout.")
                , rawTable = def &= args -- the raw key=value pairs
                , list     = def
                           &= help ("List the set of keys in the input"++
                                    " template. This is mutually exclusive"++
                                    " with output to stdout.")
                , inplace  = def
                           &= help ("Populate the source template in-place,"++
                                    " making destructive changes. This is"++
                                    " mutually exclusive with output to stdout."++
                                    " inplace is not yet supported with directory "++
                                    " template sources.")
                , prefix = def
                           &= help "Specify a custom prefix for the tagged keys"
                           &= groupname customTags &= explicit &= name "prefix"
                           &= typ "\"<<<\""
                , suffix = def
                           &= help "Specify a custom suffix for the tagged keys"
                           &= groupname customTags &= explicit &= name "suffix"
                           &= typ "\">>>\""
                } &= summary versionString &= details detailsHeader &= program "newt"

customTags :: String
customTags = "\nCustomizing tag syntax"

versionString :: String
versionString = "newt " ++ showVersion version

detailsHeader :: [String]
detailsHeader = [ "For example:"
                , ""
                , "  Transform in.cabal according to a set of key=value assignments:"
                , "  $ newt --source=in.cabal --dest=FooApp.cabal name=FooApp "++
                  "author=\"Your Name\""
                , ""
                , "  List the tagged keys in in.cabal:"
                , "  $ newt --source=in.cabal --list"
                , ""
                , "  List the tagged keys in in.cabal, using cat and stdin:"
                , "  $ cat in.cabal | newt --list"
                ]
main :: IO ()
main = do conf <- do args <- getArgs
                     -- if no arguments were specified, print help and exit:
                     case args of
                       [] -> withArgs ["--help"] $ cmdArgs config
                       _  -> cmdArgs config

          simpleTag <- mkSimpleTag $ tagBrackets conf
          let table                    = mapMaybe strToPair $ rawTable conf
              replace                  = replaceTable table
              replacement input output = replaceFile simpleTag replace input output

          res <- runErrorT $ do
                   inSpec <- inputSpec $ source conf
                   if (list conf)
                      then liftIO $ printTags simpleTag inSpec
                      else do outSpec <- outputSpec (inplace conf) inSpec $ dest conf
                              liftIO $ replacement inSpec outSpec
          case res of
            Left err -> do putStrLn err
                           exitWith (ExitFailure 1)
            Right _  -> return ()

tagBrackets :: Config -> (String, String)
tagBrackets conf = ( fromMaybe defaultPrefix $ prefix conf,
                     fromMaybe defaultSuffix $ prefix conf)

printTags :: Tag a => a -> InputSpec -> IO ()
printTags tag StandardIn         = do content <- hGetContents stdin
                                      mapM_ putStrLn $ Set.toList $ getTags tag content
printTags tag (In.TxtFile file)  = do tagSet <- getTagsFile tag file
                                      mapM_ putStrLn $ Set.toList tagSet
printTags tag (In.Directory pth) = do tagSet <- getTagsDirectory tag pth
                                      mapM_ putStrLn $ Set.toList tagSet
printTags _ fmt = putStrLn ("Unsupported input format: " ++ show fmt)


printHelp :: IO ()
printHelp = putStrLn "Usage: newt <inFile> [<outFile> [key=value]]"

isPair :: String -> Bool
isPair str = '=' `elem` str

strToPair :: String -> Maybe (String, String)
strToPair str = do idx <- elemIndex '=' str
                   let (key, rawValue) = splitAt idx str
                   return (key, tail rawValue)