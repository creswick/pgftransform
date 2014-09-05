{-# LANGUAGE DeriveDataTypeable #-}
module Config where

import Data.List    ( partition )
import Data.Version ( showVersion, Version )
import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Verbosity ( whenLoud, Verbosity(..), getVerbosity )
import System.Directory ( getAppUserDataDirectory )
import System.Environment ( getArgs, withArgs )

versionString :: Version -> String
versionString version = "pgftransform " ++ showVersion version

data Config = Config { cfgPgfFile    :: FilePath
                     , cfgOutputFile :: Maybe FilePath
                     , cfgInput      :: String
                     , cfgVerb       :: Verbosity
                     } deriving (Show, Data, Typeable)

getConfig :: Version -> IO Config
getConfig version = do rawConf <- do args <- getArgs
                                     -- if no arguments were specified, print help and exit:
                                     case args of
                                       [] -> withArgs ["--help"] $ cmdArgs $ config version
                                       _  -> cmdArgs (config version)
                       v <- getVerbosity
                       return $ rawConf { cfgVerb = v }

config :: Version -> Config
config version =
         Config { cfgPgfFile = def &= name "pgf"
                           &= help ("PGF File name, relative to current directory.")
                , cfgOutputFile = Nothing &= name "out" &= help ("Output PGF file")
                , cfgInput = def &= args -- the raw key=value pairs
                , cfgVerb = Normal
                } &= summary (versionString version) &= details detailsHeader &= program "pgftransform" &= verbosity

detailsHeader :: [String]
detailsHeader = [ "For example:"
                , ""
                , "TODO: add examples"
                ]

