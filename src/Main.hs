{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad ( when )
import Control.Monad.Error (runErrorT)
import Data.List  ( elemIndex )
import Data.Maybe ( mapMaybe )
import Data.Version ( showVersion )
import qualified Data.Set as Set

import System.Console.CmdArgs.Implicit

import Paths_newt ( version )

import Newt.Newt
import Newt.Inputs

data Config = Config { source    :: Maybe FilePath
                     , dest      :: Maybe FilePath
                     , rawTable  :: [String]
                     , list      :: Bool
                     } deriving (Show, Data, Typeable)

config :: Config
config = Config { source = def &= help "Template source location"
                , dest   = def &= help "Destination location"
                , rawTable  = def &= args --  &= help "The list of \"key=value\" pairs to use."
                , list   = def &= help "List the set of keys in the input template."
                } &= summary versionString &= details detailsHeader &= program "newt"

versionString :: String
versionString = "newt " ++ showVersion version

detailsHeader :: [String]
detailsHeader = [ "For example:"
                , "  $ newt --source=in.cabal --dest=FooApp.cabal name=FooApp author=\"Rogan Creswick\" authoremail=creswick@someemail.com"
                , ""
                ]
main :: IO ()
main = do conf <- cmdArgs config
          simpleTag <- mkSimpleTag "<<<" ">>>"
          let table                    = mapMaybe strToPair $ rawTable conf
              replacement input output = replaceFile simpleTag table input output

          case source conf of
            Nothing -> putStrLn "Stream input is not yet supported."
            Just i  -> do eInSpec <- runErrorT $ inputSpec i
                          case eInSpec of
                            Left err     -> putStrLn err
                            Right inSpec -> do maybe (outputError conf)
                                                     (\o -> replacement inSpec o)
                                                     (dest conf)
                                               when (list conf) $ printTags simpleTag inSpec

outputError :: Config -> IO ()
outputError conf = when (not $ list conf) (putStrLn "Stream output is not yet supported.")

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