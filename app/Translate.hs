module Translate (translate) where

import Bcomp
  ( Addr (..),
    BcompAsm,
    Op (..),
  )
import Control.Monad.Except (throwError)
import Data.Maybe (fromMaybe)
import Language.AST (Expr (..), LogicExpr (..), Stmt (..))
import Mangle (nextUniqueLabel)
import Translate.Layout (buildDataSection, constAddr)
import Translator.Context
  ( TranslationError (TENotImplemented),
    TranslatorM,
  )

translate :: (TranslatorM m) => Stmt -> m BcompAsm
translate root = do
  prog <- translateStmt root
  let asm = buildDataSection root ++ [OP_LABEL "START"] ++ prog
  if null asm || last asm /= OP_HLT
    then
      pure $ asm ++ [OP_HLT]
    else
      pure asm

translateStmt :: (TranslatorM m) => Stmt -> m BcompAsm
translateStmt stmt = case stmt of
  SAssign v e -> translateStmt $ SMod v e
  SMod var expr -> pure $ translateExpr expr ++ [OP_ST $ AddrAbs var]
  SReturn expr -> pure $ translateExpr expr ++ [OP_HLT]
  (SStore _ _) -> throwError $ TENotImplemented "The BEVM has terrible addressing, no pointers yet"
  SBlock stmts -> concat <$> mapM translateStmt stmts
  SLabel label -> pure [OP_LABEL label]
  SGoto label -> pure [OP_JUMP $ AddrAbs label]
  SIf lexpr ifBranch maybeElseBranch -> translateIf lexpr ifBranch maybeElseBranch
  SWhile lexpr loopBody -> do
    loopStart <- nextUniqueLabel
    let bodyWithJump = SBlock [loopBody, SGoto loopStart]
    loopCode <- translateStmt $ SIf lexpr bodyWithJump Nothing
    pure $ OP_LABEL loopStart : loopCode
  SOut num expr -> pure $ translateExpr expr ++ [OP_OUT num]

translateIf :: (TranslatorM m) => LogicExpr -> Stmt -> Maybe Stmt -> m BcompAsm
translateIf = go
  where
    go logicExpr ifBranch maybeElseBranch = case logicExpr of
      LTrue -> translateStmt ifBranch
      LFalse -> maybe (pure []) translateStmt maybeElseBranch
      LOpEq lhs rhs -> perCondition lhs rhs ifBranch maybeElseBranch [OP_BNE]
      LOpNeq lhs rhs -> perCondition lhs rhs ifBranch maybeElseBranch [OP_BEQ]
      LOpLt lhs rhs -> perCondition lhs rhs ifBranch maybeElseBranch [OP_BGE]
      LOpGe lhs rhs -> perCondition lhs rhs ifBranch maybeElseBranch [OP_BLT]
      LOpGt lhs rhs -> perCondition lhs rhs ifBranch maybeElseBranch [OP_BEQ, OP_BLT]
      LOpLe lhs rhs ->
        go
          (LOpGt lhs rhs)
          (fromMaybe (SBlock []) maybeElseBranch)
          (Just ifBranch)

    perCondition lhs rhs ifBranch maybeElseBranch branchOps = do
      let conditionCode = translateLexpr lhs rhs
      elseLabel <- nextUniqueLabel
      ifCode <- translateStmt ifBranch
      case maybeElseBranch of
        Nothing ->
          pure $ conditionCode ++ map ($ elseLabel) branchOps ++ ifCode ++ [OP_LABEL elseLabel]
        Just (SBlock []) ->
          pure $ conditionCode ++ map ($ elseLabel) branchOps ++ ifCode ++ [OP_LABEL elseLabel]
        Just elseBranch -> do
          elseCode <- translateStmt elseBranch
          endLabel <- nextUniqueLabel
          pure $
            conditionCode
              ++ map ($ elseLabel) branchOps
              ++ ifCode
              ++ [OP_JUMP (AddrAbs endLabel)]
              ++ [OP_LABEL elseLabel]
              ++ elseCode
              ++ [OP_LABEL endLabel]

translateLexpr :: Expr -> Expr -> BcompAsm
translateLexpr a' b' = case (a', b') of
  (EConst 0, a) -> translateExpr a
  (a, EConst 0) -> translateExpr a
  (a, EConst v) -> translateExpr a ++ [OP_CMP $ constAddr v]
  (EConst v, a) -> translateExpr a ++ [OP_CMP $ constAddr v]
  (a, EIdent v) -> translateExpr a ++ [OP_CMP $ AddrAbs v]
  (EIdent v, a) -> translateExpr a ++ [OP_CMP $ AddrAbs v]
  (a, b) -> translateExpr b ++ [OP_PUSH] ++ translateExpr a ++ [OP_CMP $ AddrStk 0] ++ popStack

translateExpr :: Expr -> BcompAsm
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
      (EOpAdd ex1 ex2) -> f ex1 ++ [OP_PUSH] ++ f ex2 ++ [OP_ADD $ AddrStk 0] ++ popStack
      --
      (EOpSub ex1 (EConst v)) -> f ex1 ++ [OP_SUB $ constAddr v]
      (EOpSub (EConst v) ex2) -> f (EOpSub ex2 $ EConst v) ++ [OP_NEG]
      (EOpSub ex1 (EIdent v)) -> f ex1 ++ [OP_SUB $ AddrAbs v]
      (EOpSub (EIdent v) ex2) -> f (EOpSub ex2 $ EIdent v) ++ [OP_NEG]
      (EOpSub ex1 ex2) -> f ex2 ++ [OP_PUSH] ++ f ex1 ++ [OP_SUB $ AddrStk 0] ++ popStack
      --
      (EOpAnd ex1 (EConst v)) -> f ex1 ++ [OP_AND $ constAddr v]
      (EOpAnd (EConst v) ex2) -> f $ EOpAnd ex2 $ EConst v
      (EOpAnd ex1 (EIdent v)) -> f ex1 ++ [OP_AND $ AddrAbs v]
      (EOpAnd (EIdent v) ex2) -> f $ EOpAnd ex2 $ EIdent v
      (EOpAnd ex1 ex2) -> f ex1 ++ [OP_PUSH] ++ f ex2 ++ [OP_AND $ AddrStk 0] ++ popStack
      --
      (EOpOr ex1 (EConst v)) -> f ex1 ++ [OP_OR $ constAddr v]
      (EOpOr (EConst v) ex2) -> f $ EOpOr ex2 $ EConst v
      (EOpOr ex1 (EIdent v)) -> f ex1 ++ [OP_OR $ AddrAbs v]
      (EOpOr (EIdent v) ex2) -> f $ EOpOr ex2 $ EIdent v
      (EOpOr ex1 ex2) -> f ex1 ++ [OP_PUSH] ++ f ex2 ++ [OP_OR $ AddrStk 0] ++ popStack
      --
      (ECall _ _) -> error "Functions not implemented"
      --
      EIn n -> [OP_IN n]

popStack :: BcompAsm
popStack = [OP_SWAP, OP_POP]
