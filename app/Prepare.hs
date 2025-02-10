module Prepare (renameVars) where

import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Parse
  ( Expr (..),
    LogicExpr (..),
    Stmt (..),
  )

type VarNameMap = Map String String

renameVars :: Stmt -> Stmt
renameVars = rename Map.empty
  where
    rename :: VarNameMap -> Stmt -> Stmt
    rename vnm x = case x of
      (SBlock stmts) -> SBlock $ inBlock vnm stmts
      (SMod var expr) -> SMod (checkAt vnm var) (forExpr vnm expr)
      (SIf lexpr ifB mbElseB) -> SIf (forLExpr vnm lexpr) (rename vnm ifB) (rename vnm <$> mbElseB)
      (SWhile lexpr block) -> SWhile (forLExpr vnm lexpr) (rename vnm block)
      (SReturn expr) -> SReturn $ forExpr vnm expr
      (SStore e1 e2) -> SStore (forExpr vnm e1) (forExpr vnm e2)
      (SAssign _ _) -> error "Declarations are considered in the context of the parent block"
      (SGoto s) -> SGoto $ "m_" ++ s
      (SLabel s) -> SLabel $ "m_" ++ s

    inBlock :: VarNameMap -> [Stmt] -> [Stmt]
    inBlock vnm arr = case arr of
      [] -> []
      (SAssign var expr : xs) ->
        let nvpm =
              if var `Map.member` vnm
                then Map.adjust incVarPrefix var vnm
                else Map.insert var ("v0_" ++ var) vnm
         in SAssign (nvpm Map.! var) (forExpr vnm expr) : inBlock nvpm xs
      (x : xs) -> rename vnm x : inBlock vnm xs

incVarPrefix :: String -> String
incVarPrefix ('v' : s) = 'v' : show num ++ "_" ++ name
  where
    (snum, name) = splitAtFirstExclusive '_' s
    num = read snum + 1 :: Integer
incVarPrefix s = error $ "Wrong pattern: " ++ s

splitAtFirstExclusive :: (Eq a) => a -> [a] -> ([a], [a])
splitAtFirstExclusive x xs =
  let (before, after) = break (== x) xs
   in (before, drop 1 after)

forExpr :: VarNameMap -> Expr -> Expr
forExpr = f
  where
    f vnm ex = case ex of
      (EConst _) -> ex
      (EIdent s) -> EIdent $ checkAt vnm s
      (ELoad e) -> ELoad $ f vnm e
      (EOpNeg e) -> EOpNeg $ f vnm e
      (EOpAsl e) -> EOpAsl $ f vnm e
      (EOpAsr e) -> EOpAsr $ f vnm e
      (EOpNot e) -> EOpNot $ f vnm e
      (EOpAdd e1 e2) -> EOpAdd (f vnm e1) (f vnm e2)
      (EOpSub e1 e2) -> EOpSub (f vnm e1) (f vnm e2)
      (EOpAnd e1 e2) -> EOpAnd (f vnm e1) (f vnm e2)
      (EOpOr e1 e2) -> EOpOr (f vnm e1) (f vnm e2)

forLExpr :: VarNameMap -> LogicExpr -> LogicExpr
forLExpr vnm le = case le of
  (LOpEq e1 e2) -> LOpEq (forExpr vnm e1) (forExpr vnm e2)
  (LOpNeq e1 e2) -> LOpNeq (forExpr vnm e1) (forExpr vnm e2)
  (LOpLt e1 e2) -> LOpLt (forExpr vnm e1) (forExpr vnm e2)
  (LOpGt e1 e2) -> LOpGt (forExpr vnm e1) (forExpr vnm e2)
  (LOpLe e1 e2) -> LOpLe (forExpr vnm e1) (forExpr vnm e2)
  (LOpGe e1 e2) -> LOpGe (forExpr vnm e1) (forExpr vnm e2)
  LTrue -> LTrue
  LFalse -> LFalse

checkAt :: VarNameMap -> String -> String
checkAt vnm s =
  if Map.member s vnm
    then vnm Map.! s
    else error $ "Unknown variable: " ++ s