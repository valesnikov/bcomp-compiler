module Optimize.PostOptimize (postOptimize) where

import Bcomp (Addr (AddrAbs, AddrFwd), BcompAsm, Op (..))

postOptimize :: BcompAsm -> BcompAsm
postOptimize = unusedLabelsOpt . singlePassOpt . afterJumpOpt

unusedLabelsOpt :: BcompAsm -> BcompAsm
unusedLabelsOpt = id -- TODO

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
