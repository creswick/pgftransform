module Main where

import System.IO
import Control.Monad.State
import Control.Monad (forever)
import System.Environment ( getArgs )
import PGF
import PGF.Binary hiding (version)
import PGF.Printer
import Data.Binary
import Text.Show.Pretty

import Language.Porter
import qualified Data.Map as Map

import Transform
import TransformInternal
import Config

import Paths_pgftransform ( version )

-- synonyms :: [(String, String)]
-- synonyms = [ ("wine", "vino")
--            , ("cheese", "cheddar")
--            , ("cheese", "brie")
--            , ("that", "thar")
--            , ("that", "the")
--            , ("is", "be")
--            , ("is", "are")
--            , ("is", "iss")
--            ]

-- transform :: State Parser ()
-- transform = do
--   trToParser $ do
--     addSynonyms synonyms
--   matchStems

languageStr = "Eng"

transform :: Transform
transform = do
  trToParser $ do
    addSynonyms  [ ("wine", "vino") ]
    --              , ("that", "thar")
    --              , ("that", "the")
    --              , ("is", "be")
    --              , ("is", "are")
    --              , ("warm", "toasty")
    --              ]
    addHyponyms [ ("cheese", "cheddar")
                , ("cheese", "brie")   ]
    -- addHypernyms [ ("cheese", "food")
    --              , ("cheese", "snack")
    --              , ("wine", "drink")
    --              , ("wine", "beverage") ]
  rewriteInput [ ("I'd", "I would")
             , ("would like", "want")
             , ("pizza", "a pizza")
             , ("a a", "a")
             ]
  matchStems

main :: IO ()
main = do
  cfg <- getConfig version
  pgf <- readPGF (cfgPgfFile cfg)

  let
    langStr = computeLangStr (cfgPgfFile cfg) languageStr
    mbLang = readLanguage langStr
    input = cfgInput cfg

  putStrLn ("Lang: "++langStr)
  putStrLn ("Parsing: \""++input++"\"")

  case mbLang of
    Nothing -> fail ("Could not parse language: "++langStr)
    Just lang  -> do
      let
        (_, st) = runState transform $ Parser pgf lang (\x->[x])
      case cfgOutputFile cfg of
        Nothing      -> do
          putStrLn "No output file specified"
        Just outFile -> do
          putStrLn ("Writing new PGF file to: "++outFile)
          encodeFile outFile $ pPgf st
          putStrLn ("PGF file saved to: "++outFile)
      forever $ do
        putStr "\n> "
        hFlush stdout
        inStr <- getLine
        mapM_ (\s->putStrLn ("\n   "++s)) $ runParser st inStr

      -- let (lex, _) = runState (do getLexicon)
      --                         (Parser pgf lang (\x->[x]))
      -- putStrLn "Lexicon"
      -- mapM_ (\l->print (l, stem l)) lex

computeLangStr :: FilePath -> String -> String
computeLangStr pgfPath lang = let
    -- TODO not system-independent (file path separator is /)
  base = reverse $ takeWhile (/= '/') (drop 4 $ reverse pgfPath)
  in base ++ lang