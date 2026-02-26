module Optimize.PreEvaluate (preEvaluate) where

import Data.Int (Int16)
import Language.AST (Expr (..), LogicExpr (..), Stmt (..))
import Optimize.Int16 (i16Add, i16And, i16Asl, i16Asr, i16Cmp, i16Neg, i16Not, i16Or, i16Sub)

preEvaluate :: Stmt -> Stmt
preEvaluate stmt
  | stmt == new = new
  | otherwise = preEvaluate new
  where
    new = evalStmt stmt

evalStmt :: Stmt -> Stmt
evalStmt stmt = case stmt of
  SBlock [SBlock inner] -> evalStmt $ SBlock inner
  SBlock stmts -> SBlock $ inBlockEval stmts
  SReturn expr -> SReturn $ evalExpr expr
  SIf LFalse _ Nothing -> SBlock []
  SIf LFalse _ (Just elseBranch) -> evalStmt elseBranch
  SIf LTrue ifBranch _ -> evalStmt ifBranch
  SIf cond (SBlock []) Nothing ->
    if isPureLexpr cond
      then SBlock []
      else SIf (evalLexpr cond) (SBlock []) Nothing
  SIf cond ifBranch maybeElseBranch ->
    SIf (evalLexpr cond) (evalStmt ifBranch) (evalStmt <$> maybeElseBranch)
  SWhile LFalse _ -> SBlock []
  SWhile cond body ->
    SWhile (evalLexpr cond) (evalStmt body)
  SAssign name expr -> SAssign name $ evalExpr expr
  SMod name expr -> SMod name $ evalExpr expr
  SStore addr expr -> SStore (evalExpr addr) (evalExpr expr)
  SOut n val -> SOut n (evalExpr val)
  SLabel label -> SLabel label
  SGoto label -> SGoto label

inBlockEval :: [Stmt] -> [Stmt]
inBlockEval = map evalStmt . filter (/= SBlock []) . trimToReturn
  where
    trimToReturn ((SReturn expr) : _) = [SReturn expr]
    trimToReturn (x : xs) = x : trimToReturn xs
    trimToReturn [] = []

evalExpr :: Expr -> Expr
evalExpr = exprOpt . foldConstants
  where
    foldConstants expr = case expr of
      EConst val -> EConst val
      EIdent val -> EIdent val
      ELoad val -> ELoad val
      EOpNeg (EConst n) -> EConst $ i16Neg n
      EOpAsl (EConst n) -> EConst $ i16Asl n
      EOpAsr (EConst n) -> EConst $ i16Asr n
      EOpNot (EConst n) -> EConst $ i16Not n
      EOpAdd (EConst n) (EConst m) -> EConst $ i16Add n m
      EOpSub (EConst n) (EConst m) -> EConst $ i16Sub n m
      EOpAnd (EConst n) (EConst m) -> EConst $ i16And n m
      EOpOr (EConst n) (EConst m) -> EConst $ i16Or n m
      EOpNeg val -> EOpNeg $ foldConstants val
      EOpAsl val -> EOpAsl $ foldConstants val
      EOpAsr val -> EOpAsr $ foldConstants val
      EOpNot val -> EOpNot $ foldConstants val
      EOpAdd a b -> EOpAdd (foldConstants a) (foldConstants b)
      EOpSub a b ->
        let a' = foldConstants a
            b' = foldConstants b
         in if isPureExpr a' && a' == b' then EConst 0 else EOpSub a' b'
      EOpAnd a b ->
        let a' = foldConstants a
            b' = foldConstants b
         in if isPureExpr a' && a' == b' then a' else EOpAnd a' b'
      EOpOr a b ->
        let a' = foldConstants a
            b' = foldConstants b
         in if isPureExpr a' && a' == b' then a' else EOpOr a' b'
      ECall n es -> ECall n (map foldConstants es)
      EIn _ -> expr

evalLexpr :: LogicExpr -> LogicExpr
evalLexpr logicExpr = case logicExpr of
  LTrue -> LTrue
  LFalse -> LFalse
  LOpEq lhs rhs -> evalComparison LOpEq True (==) lhs rhs
  LOpNeq lhs rhs -> evalComparison LOpNeq False (/=) lhs rhs
  LOpLt lhs rhs -> evalComparison LOpLt False (<) lhs rhs
  LOpGt lhs rhs -> evalComparison LOpGt False (>) lhs rhs
  LOpLe lhs rhs -> evalComparison LOpLe True (<=) lhs rhs
  LOpGe lhs rhs -> evalComparison LOpGe True (>=) lhs rhs

evalComparison :: (Expr -> Expr -> LogicExpr) -> Bool -> (Int16 -> Int16 -> Bool) -> Expr -> Expr -> LogicExpr
evalComparison mkExpr sameExprValue cmp lhs rhs =
  let lhs' = evalExpr lhs
      rhs' = evalExpr rhs
   in case (lhs', rhs') of
        (EConst n, EConst m) -> boolToLexpr $ i16Cmp cmp n m
        _ ->
          if isPureExpr lhs' && lhs' == rhs'
            then boolToLexpr sameExprValue
            else mkExpr lhs' rhs'

exprOpt :: Expr -> Expr
exprOpt = go
  where
    go ex
      | isPureExpr ex = pureExprOpt ex
      | otherwise = case ex of
          EOpNeg x -> EOpNeg $ go x
          EOpAsl x -> EOpAsl $ go x
          EOpAsr x -> EOpAsr $ go x
          EOpNot x -> EOpNot $ go x
          EOpAdd x1 x2 -> EOpAdd (go x1) (go x2)
          EOpSub x1 x2 -> EOpSub (go x1) (go x2)
          EOpAnd x1 x2 -> EOpAnd (go x1) (go x2)
          EOpOr x1 x2 -> EOpOr (go x1) (go x2)
          ELoad x -> ELoad $ go x
          ECall n es -> ECall n (map go es)
          _ -> ex

    pureExprOpt ex = case ex of
      EOpNeg (EOpNeg a) -> a
      EOpNeg (EOpSub a b) -> EOpSub b a
      EOpAdd (EOpNeg a) (EOpNeg b) -> EOpNeg (EOpAdd a b)
      EOpAdd a (EOpNeg b) -> EOpSub a b
      EOpAdd (EOpNeg a) b -> EOpSub b a
      EOpSub (EOpNeg a) (EOpNeg b) -> EOpSub b a
      EOpSub a (EOpNeg b) -> EOpAdd a b
      EOpSub (EOpNeg a) b -> EOpNeg (EOpAdd a b)
      EOpAdd (EOpAdd a b) (EOpAdd c d) -> EOpAdd (EOpAdd (EOpAdd a b) c) d
      EOpAdd (EOpSub a b) (EOpAdd c d) -> EOpAdd (EOpAdd (EOpSub a b) c) d
      EOpAdd (EOpAdd a b) (EOpSub c d) -> EOpSub (EOpAdd (EOpAdd a b) c) d
      EOpAdd (EOpSub a b) (EOpSub c d) -> EOpSub (EOpAdd (EOpSub a b) c) d
      EOpSub (EOpAdd a b) (EOpAdd c d) -> EOpSub (EOpSub (EOpAdd a b) c) d
      EOpSub (EOpSub a b) (EOpAdd c d) -> EOpSub (EOpSub (EOpSub a b) c) d
      EOpSub (EOpAdd a b) (EOpSub c d) -> EOpAdd (EOpSub (EOpAdd a b) c) d
      EOpSub (EOpSub a b) (EOpSub c d) -> EOpAdd (EOpSub (EOpSub a b) c) d
      EOpNeg x -> EOpNeg $ go x
      EOpAsl x -> EOpAsl $ go x
      EOpAsr x -> EOpAsr $ go x
      EOpNot x -> EOpNot $ go x
      EOpAdd x1 x2 -> EOpAdd (go x1) (go x2)
      EOpSub x1 x2 -> EOpSub (go x1) (go x2)
      EOpAnd x1 x2 -> EOpAnd (go x1) (go x2)
      EOpOr x1 x2 -> EOpOr (go x1) (go x2)
      EConst _ -> ex
      EIdent _ -> ex
      ELoad x -> ELoad $ go x
      ECall n es -> ECall n (map go es)
      EIn _ -> ex

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

boolToLexpr :: Bool -> LogicExpr
boolToLexpr True = LTrue
boolToLexpr False = LFalse
