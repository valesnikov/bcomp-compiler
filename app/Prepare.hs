module Prepare (renameVars) where

import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Language.AST (Expr (..), LogicExpr (..), Stmt (..))

type VarNameMap = Map String String

renameVars :: Stmt -> Stmt
renameVars = rename Map.empty
  where
    rename :: VarNameMap -> Stmt -> Stmt
    rename scope stmt = case stmt of
      SBlock stmts -> SBlock $ inBlock scope stmts
      SMod var expr -> SMod (checkAt scope var) (forExpr scope expr)
      SIf lexpr ifBranch maybeElseBranch ->
        SIf (forLExpr scope lexpr) (rename scope ifBranch) (rename scope <$> maybeElseBranch)
      SWhile lexpr body -> SWhile (forLExpr scope lexpr) (rename scope body)
      SReturn expr -> SReturn $ forExpr scope expr
      SStore e1 e2 -> SStore (forExpr scope e1) (forExpr scope e2)
      (SAssign _ _) -> error "Declarations are considered in the context of the parent block"
      SGoto s -> SGoto $ "m_" ++ s
      SLabel s -> SLabel $ "m_" ++ s
      SOut n v -> SOut n (forExpr scope v)

    inBlock :: VarNameMap -> [Stmt] -> [Stmt]
    inBlock scope stmts = case stmts of
      [] -> []
      (SAssign var expr : rest) ->
        let updatedScope =
              if var `Map.member` scope
                then Map.adjust incVarPrefix var scope
                else Map.insert var ("v0_" ++ var) scope
         in SAssign (checkAt updatedScope var) (forExpr scope expr) : inBlock updatedScope rest
      (stmt : rest) -> rename scope stmt : inBlock scope rest

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
    f scope ex = case ex of
      EConst _ -> ex
      EIdent s -> EIdent $ checkAt scope s
      ELoad e -> ELoad $ f scope e
      EOpNeg e -> EOpNeg $ f scope e
      EOpAsl e -> EOpAsl $ f scope e
      EOpAsr e -> EOpAsr $ f scope e
      EOpNot e -> EOpNot $ f scope e
      EOpAdd e1 e2 -> EOpAdd (f scope e1) (f scope e2)
      EOpSub e1 e2 -> EOpSub (f scope e1) (f scope e2)
      EOpAnd e1 e2 -> EOpAnd (f scope e1) (f scope e2)
      EOpOr e1 e2 -> EOpOr (f scope e1) (f scope e2)
      ECall s es -> ECall (checkAt scope s) (map (f scope) es)
      EIn _ -> ex

forLExpr :: VarNameMap -> LogicExpr -> LogicExpr
forLExpr scope logicExpr = case logicExpr of
  (LOpEq e1 e2) -> LOpEq (forExpr scope e1) (forExpr scope e2)
  (LOpNeq e1 e2) -> LOpNeq (forExpr scope e1) (forExpr scope e2)
  (LOpLt e1 e2) -> LOpLt (forExpr scope e1) (forExpr scope e2)
  (LOpGt e1 e2) -> LOpGt (forExpr scope e1) (forExpr scope e2)
  (LOpLe e1 e2) -> LOpLe (forExpr scope e1) (forExpr scope e2)
  (LOpGe e1 e2) -> LOpGe (forExpr scope e1) (forExpr scope e2)
  LTrue -> LTrue
  LFalse -> LFalse

checkAt :: VarNameMap -> String -> String
checkAt scope name = case Map.lookup name scope of
  Just renamed -> renamed
  Nothing -> error $ "Unknown variable: " ++ name
