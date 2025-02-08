module Translate (translate) where

import Control.Monad.State (State, evalState, get, put)
import Data.Maybe (fromJust, fromMaybe, isNothing, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Ops
  ( Addr (..),
    BevmAst,
    CWord (..),
    Op (..),
  )
import Parse
  ( Expr (..),
    LExpr (..),
    Stmt (..),
  )

type Translator a = State Integer a

translate :: Stmt -> BevmAst
translate s = if last bevm /= OP_HLT then bevm ++ [OP_HLT] else bevm
  where
    bevm = mkConstants s ++ mkVars s ++ [OP_LABEL "START"] ++ evalState (translateStmt s) 0

translateStmt :: Stmt -> Translator BevmAst
translateStmt stmt = case stmt of
  (SAssign _ (EConst _)) -> pure [] -- is determined during initialization
  (SAssign v e) -> translateStmt $ SMod v e
  (SMod var expr) -> pure $ translateExpr expr ++ [OP_ST $ AddrAbs var]
  (SReturn expr) -> pure $ translateExpr expr ++ [OP_HLT]
  (SStore _ _) -> error "The BEVM has terrible addressing, no pointers yet"
  (SBlock stmts) -> concat <$> mapM translateStmt stmts
  (SMark mark) -> pure [OP_LABEL  mark]
  (SGoto mark) -> pure [OP_JUMP . AddrAbs $ mark]
  (SIf lexpr ifB mbElseB) -> translateIf lexpr ifB mbElseB
  (SWhile lexpr block) -> do
    mark <- nextBranchLabel
    let bodyWithJump = SBlock [block, SGoto mark]
    body <- translateStmt $ SIf lexpr bodyWithJump Nothing
    return $ OP_LABEL mark : body

translateIf :: LExpr -> Stmt -> Maybe Stmt -> Translator BevmAst
translateIf = f
  where
    f le ifB mbElseB = case le of
      LTrue -> translateStmt ifB
      LFalse -> translateStmt $ fromMaybe (SBlock []) mbElseB
      LOpEq a b -> perCond a b ifB mbElseB [OP_BNE]
      LOpNeq a b -> perCond a b ifB mbElseB [OP_BEQ]
      LOpLt a b -> perCond a b ifB mbElseB [OP_BGE]
      LOpGe a b -> perCond a b ifB mbElseB [OP_BLT]
      LOpGt a b -> perCond a b ifB mbElseB [OP_BEQ, OP_BLT]
      LOpLe a b -> f (LOpGt a b) (fromMaybe (SBlock []) mbElseB) (Just ifB)

    perCond e1 e2 ifB mbElseB cnds = do
      let cond = translateLexpr e1 e2
      m1 <- nextBranchLabel
      ifBody <- translateStmt ifB
      if isNothing mbElseB || Just (SBlock []) == mbElseB
        then do
          return $ cond ++ map (\x -> x m1) cnds ++ ifBody ++ [OP_LABEL m1]
        else do
          elseBody <- translateStmt $ fromJust mbElseB
          m2 <- nextBranchLabel
          return $
            cond
              ++ map (\x -> x m1) cnds
              ++ ifBody
              ++ [OP_JUMP (AddrAbs m2)]
              ++ [OP_LABEL m1]
              ++ elseBody
              ++ [OP_LABEL m2]

nextBranchLabel :: Translator String
nextBranchLabel = do
  i <- get
  put $ i + 1
  return $ "l_" ++ show i

translateLexpr :: Expr -> Expr -> BevmAst
translateLexpr a' b' = case (a', b') of
  (EConst 0, a) -> translateExpr a
  (a, EConst 0) -> translateExpr a
  (a, EConst v) -> translateExpr a ++ [OP_CMP $ constAddr v]
  (EConst v, a) -> translateExpr a ++ [OP_CMP $ constAddr v]
  (a, EIdent v) -> translateExpr a ++ [OP_CMP $ AddrAbs v]
  (EIdent v, a) -> translateExpr a ++ [OP_CMP $ AddrAbs v]
  (a, b) -> translateExpr b ++ [OP_PUSH] ++ translateExpr a ++ [OP_CMP $ AddrStk 0] ++ pop_

translateExpr :: Expr -> BevmAst
translateExpr = f
  where
    f expr = case expr of
      (EConst v) -> [OP_LD $ constAddr v]
      (EIdent n) -> [OP_LD $ AddrAbs n]
      (ELoad _) -> error "The BEVM has terrible addressing, no pointers yet"
      --
      (EOpNeg ex) -> f ex ++ [OP_NEG]
      (EOpAsl ex) -> f ex ++ [OP_ASL]
      (EOpAsr ex) -> f ex ++ [OP_ASR]
      (EOpNot ex) -> f ex ++ [OP_NOT]
      --
      (EOpAdd ex1 (EConst v)) -> f ex1 ++ [OP_ADD $ constAddr v]
      (EOpAdd (EConst v) ex2) -> f $ EOpAdd ex2 $ EConst v
      (EOpAdd ex1 (EIdent v)) -> f ex1 ++ [OP_ADD $ AddrAbs v]
      (EOpAdd (EIdent v) ex2) -> f $ EOpAdd ex2 $ EIdent v
      (EOpAdd ex1 ex2) -> f ex1 ++ [OP_PUSH] ++ f ex2 ++ [OP_ADD $ AddrStk 0] ++ pop_
      --
      (EOpSub ex1 (EConst v)) -> f ex1 ++ [OP_SUB $ constAddr v]
      (EOpSub (EConst v) ex2) -> f (EOpSub ex2 $ EConst v) ++ [OP_NEG]
      (EOpSub ex1 (EIdent v)) -> f ex1 ++ [OP_SUB $ AddrAbs v]
      (EOpSub (EIdent v) ex2) -> f (EOpSub ex2 $ EIdent v) ++ [OP_NEG]
      (EOpSub ex1 ex2) -> f ex2 ++ [OP_PUSH] ++ f ex1 ++ [OP_SUB $ AddrStk 0] ++ pop_
      --
      (EOpAnd ex1 (EConst v)) -> f ex1 ++ [OP_AND $ constAddr v]
      (EOpAnd (EConst v) ex2) -> f $ EOpAnd ex2 $ EConst v
      (EOpAnd ex1 (EIdent v)) -> f ex1 ++ [OP_AND $ AddrAbs v]
      (EOpAnd (EIdent v) ex2) -> f $ EOpAnd ex2 $ EIdent v
      (EOpAnd ex1 ex2) -> f ex1 ++ [OP_PUSH] ++ f ex2 ++ [OP_AND $ AddrStk 0] ++ pop_
      --
      (EOpOr ex1 (EConst v)) -> f ex1 ++ [OP_OR $ constAddr v]
      (EOpOr (EConst v) ex2) -> f $ EOpOr ex2 $ EConst v
      (EOpOr ex1 (EIdent v)) -> f ex1 ++ [OP_OR $ AddrAbs v]
      (EOpOr (EIdent v) ex2) -> f $ EOpOr ex2 $ EIdent v
      (EOpOr ex1 ex2) -> f ex1 ++ [OP_PUSH] ++ f ex2 ++ [OP_OR $ AddrStk 0] ++ pop_

constAddr :: Integer -> Addr
constAddr v
  | -128 <= v && v <= 127 = AddrFwd (fromInteger v)
  | otherwise = AddrAbs $ getConstName v

pop_ :: BevmAst
pop_ = [OP_SWAP, OP_POP]

mkConstants :: Stmt -> BevmAst
mkConstants stmt =
  concatMap
    (\c -> [OP_LABEL $ getConstName c, OP_WORD . CWord $ fromInteger c])
    constants
  where
    constants = getConstants stmt

mkVars :: Stmt -> BevmAst
mkVars stmt =
  concatMap
    (\(c, mbV) -> [OP_LABEL c, OP_WORD $ f mbV])
    vars
  where
    vars = getVars stmt
    f Nothing = CWordUnd
    f (Just n) = CWord $ fromInteger n

getVars :: Stmt -> Set (String, Maybe Integer)
getVars = go
  where
    go x = case x of
      (SAssign n (EConst v)) -> Set.singleton (n, Just v)
      (SAssign n _) -> Set.singleton (n, Nothing)
      (SIf _ b1 b2) ->
        Set.unions $
          getVars b1 : maybeToList (getVars <$> b2)
      (SWhile _ b) -> getVars b
      (SBlock stmts) -> Set.unions $ map getVars stmts
      _ -> Set.empty

getConstants :: Stmt -> Set Integer
getConstants = go
  where
    go x = case x of
      (SAssign _ (EConst _)) -> Set.empty
      (SAssign _ expr) -> fromExpr expr
      (SMod _ expr) -> fromExpr expr
      (SIf lexpr b1 b2) ->
        Set.unions $
          [fromLexpr lexpr, getConstants b1]
            ++ maybeToList (getConstants <$> b2)
      (SWhile lexpr b) -> Set.union (fromLexpr lexpr) (getConstants b)
      (SReturn expr) -> fromExpr expr
      (SStore ex1 ex2) -> Set.union (fromExpr ex1) (fromExpr ex2)
      (SBlock stmts) -> Set.unions $ map getConstants stmts
      (SMark _) -> Set.empty
      (SGoto _) -> Set.empty

    fromLexpr x = case x of
      (LOpEq e1 e2) -> Set.union (fromExpr e1) (fromExpr e2)
      (LOpNeq e1 e2) -> Set.union (fromExpr e1) (fromExpr e2)
      (LOpLt e1 e2) -> Set.union (fromExpr e1) (fromExpr e2)
      (LOpGt e1 e2) -> Set.union (fromExpr e1) (fromExpr e2)
      (LOpLe e1 e2) -> Set.union (fromExpr e1) (fromExpr e2)
      (LOpGe e1 e2) -> Set.union (fromExpr e1) (fromExpr e2)
      _ -> Set.empty

    fromExpr x = case x of
      (EConst val) -> if -128 <= val && val <= 127 then Set.empty else Set.singleton val
      (EIdent _) -> Set.empty
      (ELoad expr) -> fromExpr expr
      (EOpNeg expr) -> fromExpr expr
      (EOpAsl expr) -> fromExpr expr
      (EOpAsr expr) -> fromExpr expr
      (EOpNot expr) -> fromExpr expr
      (EOpAdd ex1 ex2) -> Set.union (fromExpr ex1) (fromExpr ex2)
      (EOpSub ex1 ex2) -> Set.union (fromExpr ex1) (fromExpr ex2)
      (EOpAnd ex1 ex2) -> Set.union (fromExpr ex1) (fromExpr ex2)
      (EOpOr ex1 ex2) -> Set.union (fromExpr ex1) (fromExpr ex2)

getConstName :: Integer -> String
getConstName val = "c_" ++ show val
