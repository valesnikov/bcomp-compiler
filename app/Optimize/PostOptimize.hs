module Optimize.PostOptimize (postOptimize) where

import Bcomp (Addr (..), BcompAsm, Op (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

postOptimize :: BcompAsm -> BcompAsm
postOptimize = unusedLabelsOpt . mergeAdjacentLabelsOpt . singlePassOpt . afterJumpOpt

mergeAdjacentLabelsOpt :: BcompAsm -> BcompAsm
mergeAdjacentLabelsOpt ops = map rewriteOp mergedOps
  where
    (mergedOps, aliases) = go ops Nothing [] Map.empty

    go [] _ acc aliasMap = (reverse acc, aliasMap)
    go (OP_LABEL l : xs) Nothing acc aliasMap = go xs (Just l) (OP_LABEL l : acc) aliasMap
    go (OP_LABEL l : xs) (Just root) acc aliasMap = go xs (Just root) acc (Map.insert l root aliasMap)
    go (x : xs) _ acc aliasMap = go xs Nothing (x : acc) aliasMap

    canonical l = case Map.lookup l aliases of
      Nothing -> l
      Just l' ->
        if l' == l
          then l
          else canonical l'

    rewriteAddr (AddrAbs l) = AddrAbs (canonical l)
    rewriteAddr (AddrRel l) = AddrRel (canonical l)
    rewriteAddr (AddrInd l) = AddrInd (canonical l)
    rewriteAddr (AddrIndI l) = AddrIndI (canonical l)
    rewriteAddr (AddrIndD l) = AddrIndD (canonical l)
    rewriteAddr a = a

    rewriteOp (OP_AND a) = OP_AND (rewriteAddr a)
    rewriteOp (OP_OR a) = OP_OR (rewriteAddr a)
    rewriteOp (OP_ADD a) = OP_ADD (rewriteAddr a)
    rewriteOp (OP_ADC a) = OP_ADC (rewriteAddr a)
    rewriteOp (OP_SUB a) = OP_SUB (rewriteAddr a)
    rewriteOp (OP_CMP a) = OP_CMP (rewriteAddr a)
    rewriteOp (OP_LOOP a) = OP_LOOP (rewriteAddr a)
    rewriteOp (OP_LD a) = OP_LD (rewriteAddr a)
    rewriteOp (OP_SWAM a) = OP_SWAM (rewriteAddr a)
    rewriteOp (OP_JUMP a) = OP_JUMP (rewriteAddr a)
    rewriteOp (OP_CALL a) = OP_CALL (rewriteAddr a)
    rewriteOp (OP_ST a) = OP_ST (rewriteAddr a)
    rewriteOp (OP_BEQ l) = OP_BEQ (canonical l)
    rewriteOp (OP_BNE l) = OP_BNE (canonical l)
    rewriteOp (OP_BMI l) = OP_BMI (canonical l)
    rewriteOp (OP_BPL l) = OP_BPL (canonical l)
    rewriteOp (OP_BHIS l) = OP_BHIS (canonical l)
    rewriteOp (OP_BLO l) = OP_BLO (canonical l)
    rewriteOp (OP_BVS l) = OP_BVS (canonical l)
    rewriteOp (OP_BVC l) = OP_BVC (canonical l)
    rewriteOp (OP_BLT l) = OP_BLT (canonical l)
    rewriteOp (OP_BGE l) = OP_BGE (canonical l)
    rewriteOp op = op

unusedLabelsOpt :: BcompAsm -> BcompAsm
unusedLabelsOpt ops = filter keep ops
  where
    usedLabels = foldl collect Set.empty ops

    keep (OP_LABEL l) = l == "START" || l `Set.member` usedLabels
    keep _ = True

    collect acc (OP_AND a) = collectAddr acc a
    collect acc (OP_OR a) = collectAddr acc a
    collect acc (OP_ADD a) = collectAddr acc a
    collect acc (OP_ADC a) = collectAddr acc a
    collect acc (OP_SUB a) = collectAddr acc a
    collect acc (OP_CMP a) = collectAddr acc a
    collect acc (OP_LOOP a) = collectAddr acc a
    collect acc (OP_LD a) = collectAddr acc a
    collect acc (OP_SWAM a) = collectAddr acc a
    collect acc (OP_JUMP a) = collectAddr acc a
    collect acc (OP_CALL a) = collectAddr acc a
    collect acc (OP_ST a) = collectAddr acc a
    collect acc (OP_BEQ l) = Set.insert l acc
    collect acc (OP_BNE l) = Set.insert l acc
    collect acc (OP_BMI l) = Set.insert l acc
    collect acc (OP_BPL l) = Set.insert l acc
    collect acc (OP_BHIS l) = Set.insert l acc
    collect acc (OP_BLO l) = Set.insert l acc
    collect acc (OP_BVS l) = Set.insert l acc
    collect acc (OP_BVC l) = Set.insert l acc
    collect acc (OP_BLT l) = Set.insert l acc
    collect acc (OP_BGE l) = Set.insert l acc
    collect acc _ = acc

    collectAddr acc (AddrAbs l) = Set.insert l acc
    collectAddr acc (AddrRel l) = Set.insert l acc
    collectAddr acc (AddrInd l) = Set.insert l acc
    collectAddr acc (AddrIndI l) = Set.insert l acc
    collectAddr acc (AddrIndD l) = Set.insert l acc
    collectAddr acc _ = acc

afterJumpOpt :: BcompAsm -> BcompAsm
afterJumpOpt = removeBetweenJumps
  where
    removeBetweenJumps [] = []
    removeBetweenJumps (OP_JUMP (AddrAbs a) : xs) = OP_JUMP (AddrAbs a) : skipUntilLabel xs
    removeBetweenJumps (x : xs) = x : removeBetweenJumps xs

    skipUntilLabel [] = []
    skipUntilLabel (OP_LABEL s : xs) = OP_LABEL s : removeBetweenJumps xs
    skipUntilLabel (_ : xs) = skipUntilLabel xs

singlePassOpt :: BcompAsm -> BcompAsm
singlePassOpt = go
  where
    go :: BcompAsm -> BcompAsm
    go [] = []
    -- jump to the same place
    go (OP_JUMP (AddrAbs a) : OP_LABEL b : xs) =
      if a == b then OP_LABEL b : go xs else OP_JUMP (AddrAbs a) : OP_LABEL b : go xs
    -- useless loading
    go (OP_ST a : OP_LD b : xs) =
      if a == b then go (OP_ST a : xs) else OP_ST a : OP_LD b : go xs
    -- remove duplicated stores
    go (OP_ST a : OP_ST b : xs) =
      if a == b then go (OP_ST a : xs) else OP_ST a : OP_ST b : go xs
    -- remove addition with 0 (ignore OR #0 because it clean V flag)
    go (OP_ADD (AddrFwd 0) : xs) = go xs
    go (OP_SUB (AddrFwd 0) : xs) = go xs
    go (OP_CMP (AddrFwd 0) : xs) = go xs
    -- easier AC cleaning
    go (OP_AND (AddrFwd 0) : xs) = OP_CLA : go xs
    go (OP_LD (AddrFwd 0) : xs) = OP_CLA : go xs
    -- simplification to INC, DEC
    go (OP_ADD (AddrFwd 1) : xs) = OP_INC : go xs
    go (OP_SUB (AddrFwd 1) : xs) = OP_DEC : go xs
    go (OP_ADD (AddrFwd (-1)) : xs) = OP_DEC : go xs
    go (OP_SUB (AddrFwd (-1)) : xs) = OP_INC : go xs
    go (x : xs) = x : go xs
