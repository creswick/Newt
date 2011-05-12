{-# LANGUAGE DeriveDataTypeable #-}
module Newt.CmdParsing
    ( Config(..)
    , getConfig
    , debug
    , versionString
    ) where

import Data.List    ( partition )
import Data.Version ( showVersion, Version )
import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Verbosity ( whenLoud, Verbosity(..), getVerbosity )
import Safe ( headMay, tailMay )
import System.Environment ( getArgs, withArgs )

import Newt.Utilities ( isValueArg )

getConfig :: Version -> IO Config
getConfig version = do rawConf <- do args <- getArgs
                                     -- if no arguments were specified, print help and exit:
                                     case args of
                                       [] -> withArgs ["--help"] $ cmdArgs $ config version
                                       _  -> cmdArgs (config version)
                       v <- getVerbosity
                       return $ relaxArgs rawConf { verb = v }

data Config = Config { source    :: Maybe FilePath
                     , dest      :: Maybe FilePath
                     , rawTable  :: [String]
                     , list      :: Bool
                     , inplace   :: Bool
                     , prefix    :: Maybe String
                     , suffix    :: Maybe String
                     , verb      :: Verbosity
                     } deriving (Show, Data, Typeable)

config :: Version -> Config
config version =
         Config { source   = def &= name "s"
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
                , verb = Normal
                } &= summary (versionString version) &= details detailsHeader &= program "newt" &= verbosity

customTags :: String
customTags = "\nCustomizing tag syntax"

versionString :: Version -> String
versionString version = "newt " ++ showVersion version

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

-- | Relax the arguments a bit so -s --sourc / etc aren't strictly
-- necessary.
--
--  If --source is not specified, use the first non-value assignment
--  in args
--
--  if --dest is also not specified, use the first non-value
--  assignment in args for dest.
--
--  if neither --source or --dest is specified, use the first
--  non-value assignment for --source and the second for --dest.
relaxArgs :: Config -> Config
relaxArgs conf = let (newSrc, newDest, newTable) = findAltSourceDest (source conf) (dest conf) (partition isValueArg $ rawTable conf)
                 in conf { source = newSrc
                         , dest = newDest
                         , rawTable = newTable
                         }

findAltSourceDest :: Maybe FilePath -> Maybe FilePath -> ([String], [String]) -> (Maybe FilePath, Maybe FilePath, [String])
findAltSourceDest mbSrc mbDest (valueArgs, nonValueArgs) =
    case (mbSrc, mbDest) of
      (Nothing, Nothing) -> ( headMay nonValueArgs
                            , tailMay nonValueArgs >>= headMay
                            , valueArgs)
      (Just s,  Nothing) -> (Just s, headMay nonValueArgs, valueArgs)
      (Nothing, Just d ) -> (headMay nonValueArgs, Just d, valueArgs)
      (Just s,  Just d ) -> (Just s, Just d, valueArgs)

debug :: Config -> String -> IO ()
debug conf msg = case verb conf of
                   Loud -> putStrLn msg
                   _    -> return ()
