module TransformInternal where

import Control.Monad (when)
import System.Environment ( getArgs )
import PGF
import PGF.Data
import PGF.TrieMap (TrieMap)
import Data.Binary
import Data.Array
import Data.IntSet (IntSet)
import Data.Maybe
import qualified Data.Array.Unboxed as UA
import qualified Data.Set as Set
import qualified Data.IntMap as IMap
import qualified Data.Map as Map

import Text.Show.Pretty
import Utilities

import Debug.Trace

data Mutation = Mutate Language (Concr -> Concr)


modify :: PGF -> Mutation -> PGF
modify pgf (Mutate lang fn) = let
  newConcretes = case Map.lookup lang (concretes pgf) of
    Nothing -> concretes pgf
    Just c -> Map.insert lang (fn c) (concretes pgf)
  in pgf {concretes = newConcretes}

removeAllPproductions :: PGF -> Language -> PGF
removeAllPproductions pgf lang = let
  newConcretes = case Map.lookup lang (concretes pgf) of
    Nothing -> concretes pgf
    Just c -> Map.insert lang (removePproductions c) (concretes pgf)
  in pgf {concretes = newConcretes}

removePproductions :: Concr -> Concr
removePproductions concr = concr { pproductions = IMap.empty }

addSynonym :: Language -> String -> String -> Mutation
addSynonym lang old new = Mutate lang fn
  where fn :: Concr -> Concr
        fn = modifyConcrete old new

funcTree pgf lang = [(c, map (showPrintName pgf lang) $ functionsByCat pgf c) | c <- categories pgf]

toString pgf lang cid = linearize pgf lang $ mkApp cid []

instance Show CncCat where
  show (CncCat x y arr) = "\"CncCat " ++ show x ++ " " ++ show y ++ "\""
instance (Show a, Show b) => Show (TrieMap a b)

instance Show Concr where
  show c = "Concr {" ++
             " cflags = " ++ (show $ cflags c) ++
--             ", printnames = " ++ (show $ printnames c) ++
--             ", cncfuns = " ++ (show $ cncfuns c) ++
--             ", lindefs = " ++ (show $ lindefs c) ++
             ", sequences = " ++ (show $ sequences c) ++
             ", productions = " ++ (show $ productions c) ++
             ", pproductions = " ++ (show $ pproductions c) ++
--             ", lproductions = " ++ (show $ lproductions c) ++
--             ", cnccats = " ++ (show $ cnccats c) ++
--             ", lexicon = " ++ (show $ lexicon c) ++
             ", totalCats = " ++ (show $ totalCats c) ++
            " }"

-- Notes:
--
-- Adding alternates:
--  * Add new sequence (eg: S10 := "vino")
--  * Add new lin (eg: F13 := (S10) [Wine]) -- a cncfun, in the concr structure.
--  * add new production (eg: C2 -> F13[])
data VariantTemplate = VarT { vtSequence :: Sequence
                            , vtFunCId :: (FunId, CncFun)
                            , vtCatId :: FId
                            , vtProd :: Production
                            , vtOldSeqId :: SeqId
                            } deriving (Show)

modifyConcrete :: String -> String -> Concr -> Concr
modifyConcrete oldStr newStr conc = let
  -- find the sequences that the oldStr is contained in.
  oldSequences :: [(Sequence, DotPos, SeqId)]
  oldSequences = findSequences oldStr (sequences conc)

  -- for each sequence we found, create a new sequence using
  -- the newStr instead of oldStr
  --
  -- Now, for each sequence we create, we must know:
  --   * the sequence id (determined in the next step)
  --   * The category id (a FId)
  --   * The CId of the function (eg: mkCId "Wine")
  --   * The production that is to be "cloned"
  additionalSequences :: [VariantTemplate]
  additionalSequences = genNewSequences conc oldStr newStr oldSequences
  -- the seqIds list returned by updateArray is parallel to the additionalSequences list
  (newSequences, seqIds) = updateArray (sequences conc) $ map vtSequence additionalSequences

  -- now, for each new seqid, we need to create a new function:
  -- Note that this list is /also/ parallel to additionalSequences
  additionalCncFuns :: [(FunId, CncFun)]
  additionalCncFuns = zipWith mkFun seqIds additionalSequences

  -- update the array of cncfuns, noting the new ids:
  -- funIds: still parallel to additionalSequences
  (newCncfuns, funIds) = updateArray (cncfuns conc) $ map snd additionalCncFuns

  addProduction (funId, template) prods =
    updateIntMap (vtCatId template) (mkProduction funId $ vtProd template) prods

  -- make a new production for each funId from the last step,
  -- using the category id and the production template
  newPproductions = foldr addProduction (pproductions conc) (zip funIds additionalSequences)

  -- I think I need to add the other productions as well.
  newProductions = foldr addProduction (productions conc) (zip funIds additionalSequences)

  -- I *think* I'm adding a sequence that isn't being added as a sequence in a function; per.
  -- the runtime assertion: gu_assert(item->conts->lin_idx < fun->n_lins);
  --   in the C runtime, taht assert guards this array access:
  --   		PgfSequence seq = fun->lins[item->conts->lin_idx];
  in conc { sequences = newSequences
          , cncfuns = newCncfuns
          , pproductions = newPproductions
          , productions = newProductions
          }


mkFunProdMap :: IMap.IntMap (Set.Set Production) -> Map.Map FunId (Set.Set Production)
mkFunProdMap imap = listToMultimap $ concatMap morph $ IMap.toList imap
  where
    morph :: (FId, Set.Set Production) -> [(FunId, Production)]
    morph (fid, s) = foldr (accFunId fid) [] $ Set.toList s

    accFunId :: FId -> Production -> [(FunId, Production)] -> [(FunId, Production)]
    accFunId fid prod@(PApply funid _) acc = (funid, prod):acc
    accFunId _ _  acc = acc

mkFunCatMap :: IMap.IntMap (Set.Set Production) -> Map.Map FunId (Set.Set FId)
mkFunCatMap imap = listToMultimap $ concatMap morph $ IMap.toList imap
  where
    morph :: (FId, Set.Set Production) -> [(FunId, FId)]
    morph (fid, s) = foldr (accFunId fid) [] $ Set.toList s

    accFunId :: FId -> Production -> [(FunId, FId)] -> [(FunId, FId)]
    accFunId fid (PApply funid _) acc = (funid, fid):acc
    accFunId _ _  acc = acc

-- | create a mapping from sequence ID back to the function id that 
-- uses that sequence id.
mkSeqFunMap :: Array FunId CncFun -> Map.Map SeqId (Set.Set (FunId, CncFun))
mkSeqFunMap arr = listToMultimap $ concatMap morph $ assocs arr
  where
    morph :: (FunId, CncFun) -> [(SeqId, (FunId, CncFun))]
    morph (fid, fun@(CncFun cid arr)) = map (\sid->(sid, (fid, fun))) $ UA.elems arr

-- | Search for sequences that contain the specified string.
-- return the matching sequences, with the index into the sequence.
findSequences :: String -> Array SeqId Sequence -> [(Sequence, DotPos, SeqId)]
findSequences str arr = foldr find [] $ assocs arr
  where
    find :: (SeqId, Sequence) -> [(Sequence, DotPos, SeqId)] -> [(Sequence, DotPos, SeqId)]
    find (seqId, sArr) acc = acc ++ (map (\(pos,_) -> (sArr, pos, seqId)) $
                                         (filter hasTerm $ assocs sArr))

    hasTerm :: (DotPos, Symbol) -> Bool
    hasTerm (_, SymKS token)       = str == token
    hasTerm (_, SymKP tokens alts) = False -- TODO probably not false.
    hasTerm _                      = False

-- | Generate a new sequence by replacing the old string in a given sequence.
-- returns the CId, FId and Production to be "cloned"
genNewSequences :: Concr -> String -> String -> [(Sequence, DotPos, SeqId)] -> [VariantTemplate]
genNewSequences conc oldStr newStr seqs = concatMap morphSeq seqs
  where
    morphSeq :: (Sequence, DotPos, SeqId) -> [VariantTemplate]
    morphSeq (sArr, pos, seqId) = case sArr!pos of
      (SymKS token) -> let
        sequence = sArr//[(pos, SymKS $ swap oldStr newStr token)]
        cids = findCIds conc seqId
        fids = findFIds conc seqId
        prods = findProds conc seqId
        in [VarT sequence cid fid prod seqId | cid <- cids, fid <- fids, prod <- prods]
      sym            -> []

    swap :: Eq a => a -> a -> a -> a
    swap old new tok | tok == old = new
                     | otherwise  = tok



-- | locate the productions that use the given sequence.
findProds :: Concr -> SeqId -> [Production]
findProds conc seqId = let
  seqFunMap :: Map.Map SeqId (Set.Set (FunId, CncFun))
  seqFunMap = mkSeqFunMap (cncfuns conc)

  funProdMap :: Map.Map FunId (Set.Set Production)
  funProdMap = mkFunProdMap (productions conc)

  funIds :: [FunId]
  funIds = Set.toList $ Set.map fst $ Map.findWithDefault Set.empty seqId seqFunMap

  prodsForFun funid = Set.toList $ Map.findWithDefault Set.empty funid funProdMap
  in concatMap prodsForFun funIds


-- | locate the function identifiers that can generate the given old sequence.
findCIds :: Concr -> SeqId -> [(FunId, CncFun)]
findCIds conc seqId = let
  seqFunMap :: Map.Map SeqId (Set.Set (FunId, CncFun))
  seqFunMap = mkSeqFunMap (cncfuns conc)
  in Set.toList $ Map.findWithDefault Set.empty seqId seqFunMap

-- | locate the category ids that the sequence can be a part of.
findFIds :: Concr -> SeqId -> [FId]
findFIds conc seqId = let
  seqFunMap :: Map.Map SeqId (Set.Set (FunId, CncFun))
  seqFunMap = mkSeqFunMap (cncfuns conc)

  funCatMap :: Map.Map FunId (Set.Set FId)
  funCatMap = mkFunCatMap (productions conc)

  funIds :: [FunId]
  funIds = Set.toList $ Set.map fst $ Map.findWithDefault Set.empty seqId seqFunMap
  in concatMap (\funId -> Set.toList $ Map.findWithDefault Set.empty funId funCatMap) funIds


updateLProdMap :: CId -> Production -> Map.Map CId (IMap.IntMap (Set.Set Production)) ->
                  Map.Map CId (IMap.IntMap (Set.Set Production))
updateLProdMap cid prod m = case Map.lookup cid m of
  Nothing -> m
  Just intMap -> Map.insert cid (updateIntMap 2 prod intMap) m

updateIntMap :: Int -> Production ->
                IMap.IntMap (Set.Set Production) ->
                IMap.IntMap (Set.Set Production)
updateIntMap pid production imap =
  case IMap.lookup pid imap of
    Nothing  -> IMap.insert pid (Set.singleton production) imap
    Just val -> IMap.insert pid (Set.insert production val) imap

updateFunctions :: Array FunId CncFun -> [(FunId, CncFun)] -> (Array FunId CncFun, [FunId])
updateFunctions arr vals = let
  old = Map.fromList $ assocs arr
  new = Map.fromList vals
  updated = Map.union new old -- preffer mappings in 'new'
  bounds = (minimum $ Map.keys updated, maximum $ Map.keys updated)
  in (array bounds $ Map.toList updated, []) -- TODO empty list here is not correct if we must add new funs!

-- updateArray :: forall i e. (Enum i, Ix i) => e -> Array i e -> (Array i e, [i])
updateArray arr vals = let
  (low, high) = bounds arr
  oldVals = elems arr
  newHigh = high + (length vals)
  in (listArray (low, newHigh) (oldVals ++ vals), [ (high + 1) .. newHigh])

updateUArray arr vals = let
  (low, high) = UA.bounds arr
  oldVals = UA.elems arr
  newHigh = high + (length vals)
  in (UA.listArray (low, newHigh) (oldVals ++ vals), [ (high + 1) .. newHigh])

-- TODO I think I need to know the function ID that is being updated so
-- that it can be spliced back into the cncfuns array properly,
-- otherwise the function will get a new ID, and nothing will happen.
mkFun :: SeqId -> VariantTemplate -> (FunId, CncFun)
mkFun seqId varTemp = case vtFunCId varTemp of
  (id, CncFun cid arr) -> (id, CncFun cid $ arrayReplaceVal arr (vtOldSeqId varTemp) seqId)
--  (id, CncFun cid arr) -> (id, CncFun cid $ fst $ updateUArray arr [seqId])
  x -> x

-- CncFun (vtFunCId varTemp) (UA.listArray (0,0) [seqId])

arrayReplaceVal arr old new = UA.array (UA.bounds arr) $ map (swap old new) (UA.assocs arr)
  where
    swap old new (i, val) | val == old = (i, new)
                          | otherwise  = (i, val)


mkProduction :: FunId -> Production -> Production
mkProduction fid (PApply _ args) = PApply fid args
mkProduction _    prod           = prod