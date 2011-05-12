module Main where

import Control.Monad.Error (runErrorT, liftIO)
import Data.List  ( elemIndex )
import Data.Maybe ( mapMaybe, fromMaybe )
import qualified Data.Set as Set


import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( hGetContents, stdin )


import Newt.Newt
import Newt.CmdParsing ( getConfig, Config(..), debug )
import Newt.Inputs
import qualified Newt.Inputs as In
import Newt.Outputs
import qualified Newt.Outputs as Out

import Paths_newt ( version )

main :: IO ()
main = do conf <- getConfig version
          let simpleTag                = mkSimpleTag $ tagBrackets conf
              table                    = mapMaybe strToPair $ rawTable conf
              replace                  = replaceTable table
              replacement input output = replaceFile simpleTag replace input output

          debug conf ("Using configuration: "++show conf)

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


strToPair :: String -> Maybe (String, String)
strToPair str = do idx <- elemIndex '=' str
                   let (key, rawValue) = splitAt idx str
                   return (key, tail rawValue)