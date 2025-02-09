module Optimize (preEvaluate, postOptimize) where

import Data.Bits (Bits (complement, (.&.), (.|.)))
import Ops (Addr (AddrAbs, AddrFwd), BcompAsm, Op (..))
import Parse (Expr (..), LExpr (..), Stmt (..))

preEvaluate :: Stmt -> Stmt
preEvaluate stmt
  | stmt == new = new
  | otherwise = preEvaluate new
  where
    new = evalStmt stmt

evalStmt :: Stmt -> Stmt
evalStmt x = case x of
  (SBlock [SBlock as]) -> evalStmt $ SBlock as
  (SBlock stmts) -> SBlock $ inBlockEval stmts
  (SReturn expr) -> SReturn $ evalExpr expr
  (SIf LFalse _ Nothing) -> SBlock []
  (SIf LFalse _ (Just elseB)) -> elseB
  (SIf LTrue ifB _) -> ifB
  (SIf _ (SBlock []) Nothing) -> SBlock []
  (SIf cond ifB elseB) ->
    SIf (evalLexpr cond) (evalStmt ifB) (evalStmt <$> elseB)
  (SWhile LFalse _) -> SBlock []
  (SWhile cond block) ->
    SWhile (evalLexpr cond) (evalStmt block)
  (SAssign name expr) -> SAssign name $ evalExpr expr
  (SMod name expr) -> SMod name $ evalExpr expr
  (SStore addr expr) -> SStore (evalExpr addr) (evalExpr expr)
  (SLabel _) -> x
  (SGoto _) -> x

inBlockEval :: [Stmt] -> [Stmt]
inBlockEval = map evalStmt . filter (/= SBlock []) . trimToReturn []
  where
    trimToReturn acc ((SReturn expr) : _) = acc ++ [SReturn expr]
    trimToReturn acc (x : xs) = trimToReturn (acc ++ [x]) xs
    trimToReturn acc [] = acc

evalExpr :: Expr -> Expr
evalExpr = exprDisclosure . f
  where
    f x = case x of
      (EConst val) -> EConst val
      (EIdent val) -> EIdent val
      (ELoad val) -> ELoad val
      (EOpNeg (EConst n)) -> EConst (-n)
      (EOpAsl (EConst n)) -> EConst (n * 2)
      (EOpAsr (EConst n)) -> EConst (n `div` 2)
      (EOpNot (EConst n)) -> EConst (complement n)
      (EOpAdd (EConst n) (EConst m)) -> EConst (m + n)
      (EOpSub (EConst n) (EConst m)) -> EConst (m - n)
      (EOpAnd (EConst n) (EConst m)) -> EConst (m .&. n)
      (EOpOr (EConst n) (EConst m)) -> EConst (m .|. n)
      (EOpNeg val) -> EOpNeg $ f val
      (EOpAsl val) -> EOpAsl $ f val
      (EOpAsr val) -> EOpAsr $ f val
      (EOpNot val) -> EOpNot $ f val
      (EOpAdd a b) -> EOpAdd (f a) (f b)
      (EOpSub a b) -> if a == b then EConst 0 else EOpSub (f a) (f b)
      (EOpAnd a b) -> if a == b then a else EOpAnd (f a) (f b)
      (EOpOr a b) -> if a == b then a else EOpOr (f a) (f b)

evalLexpr :: LExpr -> LExpr
evalLexpr x = case x of
  LTrue -> LTrue
  LFalse -> LFalse
  (LOpEq (EConst n) (EConst m)) -> if n == m then LTrue else LFalse
  (LOpEq v1 v2) -> if v1 == v2 then LTrue else LOpEq (evalExpr v1) (evalExpr v2)
  (LOpNeq (EConst n) (EConst m)) -> if n /= m then LTrue else LFalse
  (LOpNeq v1 v2) -> if v1 == v2 then LFalse else LOpNeq (evalExpr v1) (evalExpr v2)
  (LOpLt (EConst n) (EConst m)) -> if n < m then LTrue else LFalse
  (LOpLt v1 v2) -> if v1 == v2 then LFalse else LOpLt (evalExpr v1) (evalExpr v2)
  (LOpGt (EConst n) (EConst m)) -> if n > m then LTrue else LFalse
  (LOpGt v1 v2) -> if v1 == v2 then LFalse else LOpGt (evalExpr v1) (evalExpr v2)
  (LOpLe (EConst n) (EConst m)) -> if n <= m then LTrue else LFalse
  (LOpLe v1 v2) -> if v1 == v2 then LTrue else LOpLe (evalExpr v1) (evalExpr v2)
  (LOpGe (EConst n) (EConst m)) -> if n >= m then LTrue else LFalse
  (LOpGe v1 v2) -> if v1 == v2 then LTrue else LOpGe (evalExpr v1) (evalExpr v2)

exprDisclosure :: Expr -> Expr
exprDisclosure = f
  where
    f ex = case ex of
      (EOpNeg (EOpNeg a)) -> a -- -(-a) -> a
      (EOpNeg (EOpSub a b)) -> EOpSub b a -- -(a-b) -> b-a
      --
      (EOpAdd (EOpNeg a) (EOpNeg b)) -> EOpNeg (EOpAdd a b) -- (-a) + (-b) -> -(a+b)
      (EOpAdd a (EOpNeg b)) -> EOpSub a b -- a + (-b) -> a - b
      (EOpAdd (EOpNeg a) b) -> EOpSub b a -- (-a) + b -> b - a
      --
      (EOpSub (EOpNeg a) (EOpNeg b)) -> EOpSub b a -- (-a) - (-b) -> b - a
      (EOpSub a (EOpNeg b)) -> EOpAdd a b -- a - (-b) -> a + b
      (EOpSub (EOpNeg a) b) -> EOpNeg (EOpAdd a b) -- (-a) - b -> -(a + b)
      --
      (EOpAdd (EOpAdd a b) (EOpAdd c d)) -> EOpAdd (EOpAdd (EOpAdd a b) c) d -- (a+b)+(c+d) -> ((a+b)+c)+d
      (EOpAdd (EOpSub a b) (EOpAdd c d)) -> EOpAdd (EOpAdd (EOpSub a b) c) d -- (a-b)+(c+d) -> ((a-b)+c)+d
      (EOpAdd (EOpAdd a b) (EOpSub c d)) -> EOpSub (EOpAdd (EOpAdd a b) c) d -- (a+b)+(c-d) -> ((a+b)+c)-d
      (EOpAdd (EOpSub a b) (EOpSub c d)) -> EOpSub (EOpAdd (EOpSub a b) c) d -- (a-b)+(c-d) -> ((a-b)+c)-d
      --
      (EOpSub (EOpAdd a b) (EOpAdd c d)) -> EOpSub (EOpSub (EOpAdd a b) c) d -- (a+b)-(c+d) -> ((a+b)-c)-d
      (EOpSub (EOpSub a b) (EOpAdd c d)) -> EOpSub (EOpSub (EOpSub a b) c) d -- (a-b)-(c+d) -> ((a-b)-c)-d
      (EOpSub (EOpAdd a b) (EOpSub c d)) -> EOpAdd (EOpSub (EOpAdd a b) c) d -- (a+b)-(c-d) -> ((a+b)-c)+d
      (EOpSub (EOpSub a b) (EOpSub c d)) -> EOpAdd (EOpSub (EOpSub a b) c) d -- (a-b)-(c-d) -> ((a-b)-c)+d
      --
      (EOpNeg x) -> EOpNeg $ f x
      (EOpAsl x) -> EOpAsl $ f x
      (EOpAsr x) -> EOpAsr $ f x
      (EOpNot x) -> EOpNot $ f x
      (EOpAdd x1 x2) -> EOpAdd (f x1) (f x2)
      (EOpSub x1 x2) -> EOpSub (f x1) (f x2)
      (EOpAnd x1 x2) -> EOpAnd (f x1) (f x2)
      (EOpOr x1 x2) -> EOpOr (f x1) (f x2)
      (EConst _) -> ex
      (EIdent _) -> ex
      (ELoad _) -> ex

postOptimize :: BcompAsm -> BcompAsm
postOptimize = singlePassOpt . afterJumpOpt

removeUnusedLabels :: BcompAsm -> BcompAsm
removeUnusedLabels = error "TODO"

afterJumpOpt :: BcompAsm -> BcompAsm
afterJumpOpt = removeBetweenJumps
  where
    removeBetweenJumps [] = []
    removeBetweenJumps (OP_JUMP (AddrAbs a) : xs) = OP_JUMP (AddrAbs a) : skipUntilLabel xs
    removeBetweenJumps (x:xs) = x : removeBetweenJumps xs

    skipUntilLabel [] = []
    skipUntilLabel (OP_LABEL s : xs) = OP_LABEL s : removeBetweenJumps xs
    skipUntilLabel (_ : xs) = skipUntilLabel xs

singlePassOpt :: BcompAsm -> BcompAsm
singlePassOpt = f
  where
    f :: BcompAsm -> BcompAsm
    f [] = []
    -- jump to the same place
    f (OP_JUMP (AddrAbs a) : OP_LABEL b : xs) =
      if a == b then OP_LABEL b : f xs else OP_JUMP (AddrAbs a) : OP_LABEL b : f xs
    -- useless loading
    f (OP_ST a : OP_LD b : xs) =
      if a == b then f $ OP_ST a : xs else OP_ST a : OP_LD b : f xs
    --
    f (OP_ST a : OP_ST b : xs) =
      if a == b then  f $ OP_ST a : xs else OP_ST a : OP_ST b : f xs
    -- remove addition with 0 (ignore OR #0 because it clean V flag)
    f (OP_ADD (AddrFwd 0) : xs) = f xs
    f (OP_SUB (AddrFwd 0) : xs) = f xs
    f (OP_CMP (AddrFwd 0) : xs) = f xs
    -- easier AC cleaning
    f (OP_AND (AddrFwd 0) : xs) = OP_CLA : f xs
    f (OP_LD (AddrFwd 0) : xs) = OP_CLA : f xs
    -- simplification to INC, DEC
    f (OP_ADD (AddrFwd 1) : xs) = OP_INC : f xs
    f (OP_SUB (AddrFwd 1) : xs) = OP_DEC : f xs
    f (OP_ADD (AddrFwd (-1)) : xs) = OP_DEC : f xs
    f (OP_SUB (AddrFwd (-1)) : xs) = OP_INC : f xs
    f (x : xs) = x : f xs