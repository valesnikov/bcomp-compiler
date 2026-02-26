module Translate.Layout
  ( buildDataSection,
    constAddr,
  )
where

import Bcomp
  ( Addr (AddrAbs, AddrFwd),
    BcompAsm,
    CData (CWord, CWordUnd),
    Op (OP_LABEL, OP_WORD),
  )
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Language.AST (Expr (..), LogicExpr (..), Stmt (..))

buildDataSection :: Stmt -> BcompAsm
buildDataSection stmt = buildConstants stmt ++ buildVariables stmt

constAddr :: Integer -> Addr
constAddr value
  | -128 <= value && value <= 127 = AddrFwd (fromInteger value)
  | otherwise = AddrAbs $ getConstName value

buildConstants :: Stmt -> BcompAsm
buildConstants stmt =
  concatMap
    (\constant -> [OP_LABEL $ getConstName constant, OP_WORD . CWord $ fromInteger constant])
    (Set.toAscList $ collectConstants stmt)

buildVariables :: Stmt -> BcompAsm
buildVariables stmt =
  concatMap
    (\varName -> [OP_LABEL varName, OP_WORD CWordUnd])
    (Set.toAscList $ collectVariables stmt)

collectVariables :: Stmt -> Set String
collectVariables = go
  where
    go stmt = case stmt of
      SAssign name _ -> Set.singleton name
      SIf _ thenBranch maybeElseBranch ->
        Set.unions $
          collectVariables thenBranch : maybeToList (collectVariables <$> maybeElseBranch)
      SWhile _ body -> collectVariables body
      SBlock stmts -> Set.unions $ map collectVariables stmts
      _ -> Set.empty

collectConstants :: Stmt -> Set Integer
collectConstants = go
  where
    go stmt = case stmt of
      SAssign _ expr -> constantsFromExpr expr
      SMod _ expr -> constantsFromExpr expr
      SIf logicExpr thenBranch maybeElseBranch ->
        Set.unions $
          [constantsFromLogicExpr logicExpr, collectConstants thenBranch]
            ++ maybeToList (collectConstants <$> maybeElseBranch)
      SWhile logicExpr body -> Set.union (constantsFromLogicExpr logicExpr) (collectConstants body)
      SReturn expr -> constantsFromExpr expr
      SStore lhs rhs -> Set.union (constantsFromExpr lhs) (constantsFromExpr rhs)
      SBlock stmts -> Set.unions $ map collectConstants stmts
      SLabel _ -> Set.empty
      SGoto _ -> Set.empty
      SOut _ expr -> constantsFromExpr expr

constantsFromLogicExpr :: LogicExpr -> Set Integer
constantsFromLogicExpr logicExpr = case logicExpr of
  LOpEq lhs rhs -> Set.union (constantsFromExpr lhs) (constantsFromExpr rhs)
  LOpNeq lhs rhs -> Set.union (constantsFromExpr lhs) (constantsFromExpr rhs)
  LOpLt lhs rhs -> Set.union (constantsFromExpr lhs) (constantsFromExpr rhs)
  LOpGt lhs rhs -> Set.union (constantsFromExpr lhs) (constantsFromExpr rhs)
  LOpLe lhs rhs -> Set.union (constantsFromExpr lhs) (constantsFromExpr rhs)
  LOpGe lhs rhs -> Set.union (constantsFromExpr lhs) (constantsFromExpr rhs)
  _ -> Set.empty

constantsFromExpr :: Expr -> Set Integer
constantsFromExpr expr = case expr of
  EConst value ->
    if -128 <= value && value <= 127
      then Set.empty
      else Set.singleton value
  EIdent _ -> Set.empty
  ELoad nested -> constantsFromExpr nested
  EOpNeg nested -> constantsFromExpr nested
  EOpAsl nested -> constantsFromExpr nested
  EOpAsr nested -> constantsFromExpr nested
  EOpNot nested -> constantsFromExpr nested
  EOpAdd lhs rhs -> Set.union (constantsFromExpr lhs) (constantsFromExpr rhs)
  EOpSub lhs rhs -> Set.union (constantsFromExpr lhs) (constantsFromExpr rhs)
  EOpAnd lhs rhs -> Set.union (constantsFromExpr lhs) (constantsFromExpr rhs)
  EOpOr lhs rhs -> Set.union (constantsFromExpr lhs) (constantsFromExpr rhs)
  ECall _ args -> Set.unions $ map constantsFromExpr args
  EIn _ -> Set.empty

getConstName :: Integer -> String
getConstName value = "c_" ++ show value
