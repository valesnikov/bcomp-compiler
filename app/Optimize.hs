module Optimize (preEvaluate, postOptimize) where

import Bcomp (Addr (AddrAbs, AddrFwd), BcompAsm, Op (..))
import Data.Bits (Bits (complement, (.&.), (.|.)))
import Data.Int (Int16)
import Defs (Expr (..), LogicExpr (..), Stmt (..))

preEvaluate :: Stmt -> Stmt
preEvaluate stmt
  | stmt == new = new
  | otherwise = preEvaluate new
  where
    new = evalStmt stmt

evalStmt :: Stmt -> Stmt
evalStmt x = case x of
  SBlock [SBlock as] -> evalStmt $ SBlock as
  SBlock stmts -> SBlock $ inBlockEval stmts
  SReturn expr -> SReturn $ evalExpr expr
  SIf LFalse _ Nothing -> SBlock []
  SIf LFalse _ (Just elseB) -> elseB
  SIf LTrue ifB _ -> ifB
  SIf cond (SBlock []) Nothing ->
    if isPureLexpr cond
      then SBlock []
      else SIf (evalLexpr cond) (SBlock []) Nothing
  SIf cond ifB elseB ->
    SIf (evalLexpr cond) (evalStmt ifB) (evalStmt <$> elseB)
  SWhile LFalse _ -> SBlock []
  SWhile cond block ->
    SWhile (evalLexpr cond) (evalStmt block)
  SAssign name expr -> SAssign name $ evalExpr expr
  SMod name expr -> SMod name $ evalExpr expr
  SStore addr expr -> SStore (evalExpr addr) (evalExpr expr)
  SOut n v -> SOut n (evalExpr v)
  SLabel _ -> x
  SGoto _ -> x

inBlockEval :: [Stmt] -> [Stmt]
inBlockEval = map evalStmt . filter (/= SBlock []) . trimToReturn
  where
    trimToReturn ((SReturn expr) : _) = [SReturn expr]
    trimToReturn (x : xs) = x : trimToReturn xs
    trimToReturn [] = []

evalExpr :: Expr -> Expr
evalExpr = exprOpt . f
  where
    f x = case x of
      (EConst val) -> EConst val
      (EIdent val) -> EIdent val
      (ELoad val) -> ELoad val
      (EOpNeg (EConst n)) -> EConst $ i16Neg n
      (EOpAsl (EConst n)) -> EConst $ i16Asl n
      (EOpAsr (EConst n)) -> EConst $ i16Asr n
      (EOpNot (EConst n)) -> EConst $ i16Not n
      (EOpAdd (EConst n) (EConst m)) -> EConst $ i16Add n m
      (EOpSub (EConst n) (EConst m)) -> EConst $ i16Sub n m
      (EOpAnd (EConst n) (EConst m)) -> EConst $ i16And n m
      (EOpOr (EConst n) (EConst m)) -> EConst $ i16Or n m
      (EOpNeg val) -> EOpNeg $ f val
      (EOpAsl val) -> EOpAsl $ f val
      (EOpAsr val) -> EOpAsr $ f val
      (EOpNot val) -> EOpNot $ f val
      (EOpAdd a b) -> EOpAdd (f a) (f b)
      (EOpSub a b) ->
        let a' = f a
            b' = f b
         in if isPureExpr a' && a' == b' then EConst 0 else EOpSub a' b'
      (EOpAnd a b) ->
        let a' = f a
            b' = f b
         in if isPureExpr a' && a' == b' then a' else EOpAnd a' b'
      (EOpOr a b) ->
        let a' = f a
            b' = f b
         in if isPureExpr a' && a' == b' then a' else EOpOr a' b'
      (ECall n es) -> ECall n (map f es)
      (EIn _) -> x

evalLexpr :: LogicExpr -> LogicExpr
evalLexpr x = case x of
  LTrue -> LTrue
  LFalse -> LFalse
  LOpEq v1 v2 ->
    let a = evalExpr v1
        b = evalExpr v2
     in case (a, b) of
          (EConst n, EConst m) -> boolToLexpr $ i16Cmp (==) n m
          _ -> if isPureExpr a && a == b then LTrue else LOpEq a b
  LOpNeq v1 v2 ->
    let a = evalExpr v1
        b = evalExpr v2
     in case (a, b) of
          (EConst n, EConst m) -> boolToLexpr $ i16Cmp (/=) n m
          _ -> if isPureExpr a && a == b then LFalse else LOpNeq a b
  LOpLt v1 v2 ->
    let a = evalExpr v1
        b = evalExpr v2
     in case (a, b) of
          (EConst n, EConst m) -> boolToLexpr $ i16Cmp (<) n m
          _ -> if isPureExpr a && a == b then LFalse else LOpLt a b
  LOpGt v1 v2 ->
    let a = evalExpr v1
        b = evalExpr v2
     in case (a, b) of
          (EConst n, EConst m) -> boolToLexpr $ i16Cmp (>) n m
          _ -> if isPureExpr a && a == b then LFalse else LOpGt a b
  LOpLe v1 v2 ->
    let a = evalExpr v1
        b = evalExpr v2
     in case (a, b) of
          (EConst n, EConst m) -> boolToLexpr $ i16Cmp (<=) n m
          _ -> if isPureExpr a && a == b then LTrue else LOpLe a b
  LOpGe v1 v2 ->
    let a = evalExpr v1
        b = evalExpr v2
     in case (a, b) of
          (EConst n, EConst m) -> boolToLexpr $ i16Cmp (>=) n m
          _ -> if isPureExpr a && a == b then LTrue else LOpGe a b

exprOpt :: Expr -> Expr
exprOpt = go
  where
    go ex
      | isPureExpr ex = pureExprOpt ex
      | otherwise = case ex of
          (EOpNeg x) -> EOpNeg $ go x
          (EOpAsl x) -> EOpAsl $ go x
          (EOpAsr x) -> EOpAsr $ go x
          (EOpNot x) -> EOpNot $ go x
          (EOpAdd x1 x2) -> EOpAdd (go x1) (go x2)
          (EOpSub x1 x2) -> EOpSub (go x1) (go x2)
          (EOpAnd x1 x2) -> EOpAnd (go x1) (go x2)
          (EOpOr x1 x2) -> EOpOr (go x1) (go x2)
          (ELoad x) -> ELoad $ go x
          (ECall n es) -> ECall n (map go es)
          _ -> ex

    pureExprOpt ex = case ex of
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
      (EOpNeg x) -> EOpNeg $ go x
      (EOpAsl x) -> EOpAsl $ go x
      (EOpAsr x) -> EOpAsr $ go x
      (EOpNot x) -> EOpNot $ go x
      (EOpAdd x1 x2) -> EOpAdd (go x1) (go x2)
      (EOpSub x1 x2) -> EOpSub (go x1) (go x2)
      (EOpAnd x1 x2) -> EOpAnd (go x1) (go x2)
      (EOpOr x1 x2) -> EOpOr (go x1) (go x2)
      (EConst _) -> ex
      (EIdent _) -> ex
      (ELoad x) -> ELoad $ go x
      (ECall n es) -> ECall n (map go es)
      (EIn _) -> ex

isPureExpr :: Expr -> Bool
isPureExpr expr = case expr of
  EConst _ -> True
  EIdent _ -> True
  ELoad x -> isPureExpr x
  EOpNeg x -> isPureExpr x
  EOpAsl x -> isPureExpr x
  EOpAsr x -> isPureExpr x
  EOpNot x -> isPureExpr x
  EOpAdd x1 x2 -> isPureExpr x1 && isPureExpr x2
  EOpSub x1 x2 -> isPureExpr x1 && isPureExpr x2
  EOpAnd x1 x2 -> isPureExpr x1 && isPureExpr x2
  EOpOr x1 x2 -> isPureExpr x1 && isPureExpr x2
  ECall _ _ -> False
  EIn _ -> False

isPureLexpr :: LogicExpr -> Bool
isPureLexpr lexpr = case lexpr of
  LTrue -> True
  LFalse -> True
  LOpEq x1 x2 -> isPureExpr x1 && isPureExpr x2
  LOpNeq x1 x2 -> isPureExpr x1 && isPureExpr x2
  LOpLt x1 x2 -> isPureExpr x1 && isPureExpr x2
  LOpGt x1 x2 -> isPureExpr x1 && isPureExpr x2
  LOpLe x1 x2 -> isPureExpr x1 && isPureExpr x2
  LOpGe x1 x2 -> isPureExpr x1 && isPureExpr x2

toI16 :: Integer -> Int16
toI16 = fromInteger

fromI16 :: Int16 -> Integer
fromI16 = toInteger

i16Neg :: Integer -> Integer
i16Neg n = fromI16 $ negate $ toI16 n

i16Asl :: Integer -> Integer
i16Asl n = fromI16 $ toI16 n * 2

i16Asr :: Integer -> Integer
i16Asr n = fromI16 $ toI16 n `div` 2

i16Not :: Integer -> Integer
i16Not n = fromI16 $ complement $ toI16 n

i16Add :: Integer -> Integer -> Integer
i16Add n m = fromI16 $ toI16 n + toI16 m

i16Sub :: Integer -> Integer -> Integer
i16Sub n m = fromI16 $ toI16 n - toI16 m

i16And :: Integer -> Integer -> Integer
i16And n m = fromI16 $ toI16 n .&. toI16 m

i16Or :: Integer -> Integer -> Integer
i16Or n m = fromI16 $ toI16 n .|. toI16 m

i16Cmp :: (Int16 -> Int16 -> Bool) -> Integer -> Integer -> Bool
i16Cmp cmp n m = cmp (toI16 n) (toI16 m)

boolToLexpr :: Bool -> LogicExpr
boolToLexpr b =
  if b
    then LTrue
    else LFalse

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
      if a == b then f $ OP_ST a : xs else OP_ST a : OP_ST b : f xs
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
