module Main where

import Data.Array
import Data.List

import PGF

import Test.HUnit
import Test.Hspec
import Test.Hspec.HUnit
import Test.QuickCheck

import Transform

synonymTests :: [( [(String, String)] , (String, [String]))]
synonymTests = [ ([], ("that cheese is warm", ["that cheese is warm"]))
               , ([("cheese", "cheddar")], ("this cheddar is warm", ["this cheese is warm"]))
               , ([("that", "thar")],      ("thar cheese is warm",  ["that cheese is warm"]))
               , ([("is", "be")],          ("that cheese be warm",  ["that cheese is warm"]))
               ]

stemFoodsTests :: [(String, [String])]
stemFoodsTests = [ ("that cheese is warm",  ["that cheese is warm"])
                 , ("that wines is warm", ["that wine is warm"])
                 ]

stemPhrasebookTests :: [(String, [String])]
stemPhrasebookTests = [ ("that cheese is warm",  ["that cheese is warm ."])
                      , ("that wines is warm", ["that wine is warm ."])
                      ]

inputRewriteTests ::  [(String, [String])]
inputRewriteTests = [ ("I'd like pizza", ["I want a pizza ."])
                    , ("I'd like a pizza", ["I want a pizza ."])
                    , ("I would like pizza", ["I want a pizza ."])
                    , ("I would like a pizza", ["I want a pizza ."])
                    , ("I want pizza", ["I want a pizza ."])
                    ]

inputRewriteTransform = do
  rewriteInput [ ("I'd", "I would")
               , ("pizza", "a pizza")
               , ("would like", "want")
               , ("a a", "a")
               ]
  trToParser $ addSynonyms [("would", "want")]

mkSynonymTest :: (Transform -> String -> [String] -> Test) ->
                 ( [(String, String)], (String, [String])) -> Spec
mkSynonymTest tester (synonyms, (input, oracle)) = fromHUnitTest
  (TestLabel ("Parses \""++input++"\"") (tester (trToParser $ addSynonyms synonyms) input oracle))

mkStemTest tester (input, oracle) = fromHUnitTest
  (TestLabel ("Parses \""++input++"\"")(tester matchStems input oracle))

mkRewriteTest :: (String -> [String] -> Test) ->
                 (String, [String]) -> Spec
mkRewriteTest tester (input, oracle) = fromHUnitTest
  (TestLabel ("Parses \""++input++"\"") (tester input oracle))

main :: IO ()
main = do
  phraseBookTester <- buildTester "grammars/Phrasebook.pgf" "PhrasebookEng"
  foodsTester <- buildTester "grammars/Foods.pgf" "FoodsEng"
  hspec $ do
  describe "Synonym tests" $ do
    describe "Foods" $ sequence_ $
      map (mkSynonymTest foodsTester) synonymTests

    describe "Phrasebook" $ sequence_ $
      map (mkSynonymTest phraseBookTester) $ addsuffix " ." synonymTests


  describe "Stemming tests" $ do
    describe "Foods tests" $ sequence_ $
      map (mkStemTest foodsTester) stemFoodsTests

    describe "Phrasebook tests" $ sequence_ $
      map (mkStemTest phraseBookTester) $ stemPhrasebookTests

  describe "Input Rewrite tests" $ sequence_ $
    map (mkRewriteTest $ phraseBookTester inputRewriteTransform) inputRewriteTests


addsuffix :: String -> [( [(String, String)] , (String, [String]))] -> [( [(String, String)] , (String, [String]))]
addsuffix _ [] = []
addsuffix suffix (x:xs) = (update x:addsuffix suffix xs)
 where
   update (syns, (inp, oracles)) = (syns, (inp, map (++suffix) oracles))

buildTester :: FilePath -> String -> IO (Transform -> String -> [String] -> Test)
buildTester pgfPath langStr = do
  pgf <- readPGF pgfPath
  case readLanguage langStr of
    Nothing   -> fail ("Could not read "++langStr)
    Just lang -> return $ \tr input oracle -> let
      results = parseWithTr pgf lang tr input
      in results ~?= oracle

--   describe "updateArray" $ do

--     it "adds an element to the array without crashing." $
--       property prop_arrayGrows

-- --genArrayTest :: e -> Array i e -> Array i e -> Test
-- prop_arrayGrows :: Int -> (Int, Int) -> Gen Prop
-- prop_arrayGrows val (low, high) = high >= low ==> let
--   arr = listArray (low, high) [low .. high]
--   newArr = updateArray val arr
--   in length (elems $ fst newArr) == 1 + length (elems arr)

-- "that cheese is warm"
-- PSentence (SProp (Is (ThatMass Cheese) (PropQuality Warm)))

-- Phrasebook> p -cat=Proposition "that cheese is warm"
-- Is (ThatMass Cheese) (PropQuality Warm)
