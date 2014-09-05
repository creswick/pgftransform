module Transform
  ( addSynonym
  , addSynonyms
  , addHyponyms
  , addHypernyms
  , rewriteInput
  , addTerm
  , matchStems
  , Tr(..)
  , Transform
  , Parser(..)
  , initParser
  , interpret
  , runParser
  , trToParser
  , getLexicon
  , parseWithTr
-- experiments

  )
where

import Debug.Trace

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Multimap as MMap
import Data.List (nub, sort)


import PGF
import PGF.Data
import PGF.Morphology
import Language.Porter

-- import Control.Monad.Applicative (<$>)
import Control.Monad.State
import qualified TransformInternal as TI

parseWithTr :: PGF -> Language -> Transform -> String -> [String]
parseWithTr pgf lang tr input = let
  (_, st) = runState tr $ initParser pgf lang
  in nub $ sort $ runParser st input

initParser :: PGF -> Language -> Parser
initParser pgf lang = Parser pgf lang (\x->[x])

type Transform = State Parser ()

-- | Description of pgf transform with specialized input
-- pre-processing.
data Parser = Parser {
  -- | The pgf object with the relaxed grammar.
  pPgf :: PGF
  -- | The language that has been relaxed.
  , pLang :: Language
  -- | the preprocessor that creates alternate input strings.
  , pPrep :: String -> [String]
  }

-- | Description of PGF-only transformations.
data Tr = Tr { trPgf :: PGF
             , trLang :: Language
             }

-- | Parse input, using a grammar transform.
interpret :: Tr -> String -> [String]
interpret (Tr pgf lang) input = let
  parseType = startCat pgf
  xs = parse pgf lang parseType input
  in map (linearize pgf lang) xs

-- | Parse input with the relaxed grammar and input pre-processor
-- defined in the parser.
runParser :: Parser -> String -> [String]
runParser p input = let theParser = interpret (Tr (pPgf p) (pLang p))
                    in nub $ sort $ concatMap theParser (pPrep p input)

-- | Allow a synonym to be accepted as another term.  This is
-- essentially the same as adding a variant to the string terminal in
-- the concrete syntax.
--
-- The first string is the existing term, the second string is the
-- newly accepted term.
addSynonym :: String -> String -> State Tr ()
addSynonym old new = modify $ \(Tr pgf lang) -> Tr {
                       trPgf = TI.modify pgf (TI.addSynonym lang old new)
                     , trLang = lang }

-- | Accept a larger mapping of synonyms.
-- This function delegates to `addSynonym`
addSynonyms :: [(String, String)] -> State Tr ()
addSynonyms mapping = mapM_ (uncurry addSynonym) mapping

-- | Accept a specific string as a specified category in the abstract
-- syntax.
--
-- The first string is the name of the target category, the second is
-- the string to accept.
addTerm :: String -> String -> State Tr ()
addTerm cat newStr = undefined -- TODO

-- | Accept terms that are more specific variants of a word in the
-- existing grammar.
--
-- Each pair is a tuple of: (Hypernym, Hyponym), where the Hypernym
-- should already exist in the grammar.
addHyponyms :: [(String, String)] -> State Tr ()
addHyponyms = addSynonyms

addHypernyms :: [(String, String)] -> State Tr ()
addHypernyms = addSynonyms


-- | Alter a simple PGF transform to a parser-based transform.
--
-- This is a type coersion to wrap simple alterations to PGF objects
-- with an input preprocessor to enable more complex mappings.
trToParser :: State Tr a -> State Parser a
trToParser trst = do
  parser <- get
  let (val, st) = runState trst Tr { trPgf = pPgf parser
                                   , trLang = pLang parser }
  put parser { pPgf = trPgf st
             , pLang = trLang st }
  return val

-- | The sledgehammer approach.
--
-- Re-write input patterns to match something that the parser accepts.
--
-- These are applied in order, so the output of one can be modified by
-- a later pattern.  This enables ugly hacks like:
--
--   rewriteInput [("pizza", "a pizza"), ("a a", "a")]
--
-- which will add the article 'a' in front of 'pizza', if it doesn't
-- exist.
rewriteInput :: [(String, String)] -> State Parser ()
rewriteInput changes = do
  parser <- get
  put parser { pPrep = (pPrep parser) >=> (rewrite changes)
             }
  where
    rewrite :: [(String, String)] -> String -> [String]
    rewrite [] input             = [input]
    rewrite ((old,new):cs) input =
      concatMap (rewrite cs) [T.unpack $ T.replace (T.pack old) (T.pack new) $ T.pack input]


-- | Parse based on stems.
--
-- Tokenizes the input on spaces, then for each input token that is
-- not in the lexicon, search the lexicon using stem-based equality,
-- generating a new variant of the input based on the lexicon tokens
-- that do match.
matchStems :: State Parser ()
matchStems = do
  lex <- getLexicon
  let
    lexSet = Set.fromList lex
    synMap = MMap.fromList $ map (\s->(stem s, s)) lex

    stemInput :: String -> [String]
    stemInput input = allPaths $ map adjustWord $ words input

    adjustWord :: String -> [String]
    adjustWord str = Set.toList $ MMap.get (stem str) synMap

  modify $ \p -> p { pPrep = (pPrep p) >=> stemInput }

allPaths :: [[String]] -> [String]
allPaths []     = []
allPaths (x:[]) = x
allPaths (x:xs) = [unwords [str, suffix] | str <- x, suffix <- (allPaths xs)]

-- | Access all the terms in the lexicon; used to build more complex
-- operations.
getLexicon :: State Parser [String]
getLexicon = do
  parser <- get
  let
    pgf = pPgf parser
    lang = pLang parser
  return $ map fst (fullFormLexicon $ buildMorpho pgf lang)

-- | Check spelling for input text.
--
-- For example, the input string:
--    "thes wine is waarm"
--
-- could be parsed as:
--    "this wine is warm"
--
spellcheck :: Parser -> Parser
spellcheck = undefined

-- | Parse in a case-insensitive manner.
--
-- For example, the input string:
--    "This Wine is warm"
--
-- could be parsed as:
--    "this wine is warm"
--
ignoreCase :: Parser -> Parser
ignoreCase = undefined

-- | Drop up to N unmatched words from the input between each parsed
-- word from the grammar.
--
-- For example, the input string:
--    "this white wine is warm"
--
-- could be parsed as:
--    "this wine is warm"
--
ignoreWords :: Int -> Parser -> Parser
ignoreWords = undefined

-- | Inject up to N expected tokens between each token in the input
-- string.
--
-- For example, the input string:
--    "this wine warm"
--
-- could be parsed as:
--    "this wine is warm"
--
addWords :: Int -> Parser -> Parser
addWords = undefined

-- | Ignore non-head tokens in the input string.
onlyHeads :: Parser -> Parser
onlyHeads = undefined

-- | Ignore adjectives in the input string.
dropAdjs :: Parser -> Parser
dropAdjs = undefined

-- | Ignore adverbs in the input string.
dropAdvs :: Parser -> Parser
dropAdvs = undefined