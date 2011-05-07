{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad ( when )
import Control.Monad.Error (runErrorT)
import Data.List  ( elemIndex )
import Data.Maybe ( mapMaybe, fromMaybe )
import Data.Version ( showVersion )
import qualified Data.Set as Set

import System.Console.CmdArgs.Implicit
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
                     , prefix    :: Maybe String
                     , suffix    :: Maybe String
                     } deriving (Show, Data, Typeable)

config :: Config
config = Config { source = def &= help "Template source location" &= name "s"
                , dest   = def &= help "Destination location"
                , rawTable  = def &= args --  &= help "The list of \"key=value\" pairs to use."
                , list   = def &= help "List the set of keys in the input template."
                , prefix = def &= help "Specify a custom prefix for the tagged keys" &= groupname customTags &=
                           explicit &= name "prefix" &= typ "\"<<<\""
                , suffix = def &= help "Specify a custom suffix for the tagged keys" &= groupname customTags&=
                           explicit &= name "suffix" &= typ "\">>>\""
                } &= summary versionString &= details detailsHeader &= program "newt"

customTags :: String
customTags = "\nCustomizing tag syntax"

versionString :: String
versionString = "newt " ++ showVersion version

detailsHeader :: [String]
detailsHeader = [ "For example:"
                , "  $ newt --source=in.cabal --dest=FooApp.cabal name=FooApp author=\"Rogan Creswick\" authoremail=creswick@someemail.com"
                , ""
                ]
main :: IO ()
main = do conf <- cmdArgs config
          simpleTag <- mkSimpleTag $ tagBrackets conf
          let table                    = mapMaybe strToPair $ rawTable conf
              replacement input output = replaceFile simpleTag table input output

          eInSpec <- runErrorT $ inputSpec $ source conf
          case eInSpec of
            Left err     -> putStrLn err
            Right inSpec -> do case (list conf) of
                                 True -> printTags simpleTag inSpec
                                 False -> do eOutSpec <- runErrorT $ outputSpec inSpec $ dest conf
                                             case eOutSpec of
                                               Left err -> putStrLn err
                                               Right outSpec -> do replacement inSpec outSpec

outputError :: Config -> IO ()
outputError conf = when (not $ list conf) (putStrLn "Stream output is not yet supported.")

tagBrackets :: Config -> (String, String)
tagBrackets conf = ( fromMaybe "<<<" $ prefix conf,
                     fromMaybe ">>>" $ prefix conf)

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