module Prepare (unblockVars) where

import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Parse
  ( Expr (..),
    LExpr (..),
    Stmt (..),
  )

type VMap = Map String String

unblockVars :: Stmt -> Stmt
unblockVars = unblockVars' Map.empty

unblockVars' :: VMap -> Stmt -> Stmt
unblockVars' vmp x = case x of
  (SBlock stmts) -> SBlock $ inBlock vmp stmts
  (SMod var expr) -> SMod (checkAt vmp var) (forExpr vmp expr)
  (SIf lexpr ifB mbElseB) -> SIf (forLExpr vmp lexpr) (unblockVars' vmp ifB) (unblockVars' vmp <$> mbElseB)
  (SWhile lexpr block) -> SWhile (forLExpr vmp lexpr) (unblockVars' vmp block)
  (SReturn expr) -> SReturn $ forExpr vmp expr
  (SStore e1 e2) -> SStore (forExpr vmp e1) (forExpr vmp e2)
  (SAssign _ _) -> error "Declarations are considered in the context of the parent block"
  (SGoto s) -> SGoto $ "m_" ++ s
  (SLabel s) -> SLabel $ "m_" ++ s

inBlock :: VMap -> [Stmt] -> [Stmt]
inBlock vmp' = reverse . fst . foldl f ([], vmp')
  where
    f (res, vmp) x = case x of
      (SAssign var expr) ->
        let nvpm =
              if var `Map.member` vmp
                then Map.adjust incrementPrefix var vmp
                else Map.insert var ("v0_" ++ var) vmp
         in (SAssign (nvpm Map.! var) (forExpr vmp expr) : res, nvpm)
      x' -> (unblockVars' vmp x' : res, vmp)

incrementPrefix :: String -> String
incrementPrefix ('v' : s) = "v" ++ show num ++ "_" ++ name
  where
    (snum, name) = splitAtFirstExclusive '_' s
    num :: Integer
    num = read snum + 1
incrementPrefix s = error $ "Wrong pattern: " ++ s

splitAtFirstExclusive :: (Eq a) => a -> [a] -> ([a], [a])
splitAtFirstExclusive x xs =
  let (before, after) = break (== x) xs
   in (before, drop 1 after)

forExpr :: VMap -> Expr -> Expr
forExpr = f
  where
    f vmp ex = case ex of
      (EConst _) -> ex
      (EIdent s) -> EIdent $ checkAt vmp s
      (ELoad e) -> ELoad $ f vmp e
      (EOpNeg e) -> EOpNeg $ f vmp e
      (EOpAsl e) -> EOpAsl $ f vmp e
      (EOpAsr e) -> EOpAsr $ f vmp e
      (EOpNot e) -> EOpNot $ f vmp e
      (EOpAdd e1 e2) -> EOpAdd (f vmp e1) (f vmp e2)
      (EOpSub e1 e2) -> EOpSub (f vmp e1) (f vmp e2)
      (EOpAnd e1 e2) -> EOpAnd (f vmp e1) (f vmp e2)
      (EOpOr e1 e2) -> EOpOr (f vmp e1) (f vmp e2)

forLExpr :: VMap -> LExpr -> LExpr
forLExpr vmp le = case le of
  (LOpEq e1 e2) -> LOpEq (forExpr vmp e1) (forExpr vmp e2)
  (LOpNeq e1 e2) -> LOpNeq (forExpr vmp e1) (forExpr vmp e2)
  (LOpLt e1 e2) -> LOpLt (forExpr vmp e1) (forExpr vmp e2)
  (LOpGt e1 e2) -> LOpGt (forExpr vmp e1) (forExpr vmp e2)
  (LOpLe e1 e2) -> LOpLe (forExpr vmp e1) (forExpr vmp e2)
  (LOpGe e1 e2) -> LOpGe (forExpr vmp e1) (forExpr vmp e2)
  tf -> tf

checkAt :: VMap -> String -> String
checkAt vmp s =
  if Map.member s vmp
    then vmp Map.! s
    else error $ "Unknown variable: " ++ s