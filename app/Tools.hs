{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Tools where

import Control.Monad.State (MonadState (get, put))
import Control.Monad.State.Class (gets)
import Data.Functor ((<&>))
import Defs (Expr (..), LogicExpr (..), TranslatorState (..))

newTraslatorState :: TranslatorState
newTraslatorState = TranslatorState {trCounter = 0, trLogs = []}

logMessage :: (MonadState TranslatorState m) => String -> m ()
logMessage msg = do
  st <- get
  put $ st {trLogs = msg : trLogs st}

getLogs :: (MonadState TranslatorState m) => m [String]
getLogs = gets trLogs

getUniqId :: (MonadState TranslatorState m) => m Integer
getUniqId = do
  st <- get
  put $ st {trCounter = trCounter st + 1}
  return $ trCounter st

mapExprConst :: (Monad m) => (Integer -> m Integer) -> Expr -> m Expr
mapExprConst f = mapExpr (f, return)

mapExprIdent :: (Monad m) => (String -> m String) -> Expr -> m Expr
mapExprIdent f = mapExpr (return, f)

mapLExprConst :: (Monad m) => (Integer -> m Integer) -> LogicExpr -> m LogicExpr
mapLExprConst f = mapLExpr (f, return)

mapLExprIdent :: (Monad m) => (String -> m String) -> LogicExpr -> m LogicExpr
mapLExprIdent f = mapLExpr (return, f)

mapExpr :: (Monad m) => (Integer -> m Integer, String -> m String) -> Expr -> m Expr
mapExpr f expression = case expression of
  EConst i -> fst f i <&> EConst
  EIdent s -> snd f s <&> EIdent
  ELoad ex -> mapExpr f ex <&> ELoad
  EOpNeg ex -> mapExpr f ex <&> EOpNeg
  EOpAsl ex -> mapExpr f ex <&> EOpAsl
  EOpAsr ex -> mapExpr f ex <&> EOpAsr
  EOpNot ex -> mapExpr f ex <&> EOpNot
  EOpAdd e1 e2 -> do
    ne1 <- mapExpr f e1
    ne2 <- mapExpr f e2
    return $ EOpAdd ne1 ne2
  EOpSub e1 e2 -> do
    ne1 <- mapExpr f e1
    ne2 <- mapExpr f e2
    return $ EOpSub ne1 ne2
  EOpAnd e1 e2 -> do
    ne1 <- mapExpr f e1
    ne2 <- mapExpr f e2
    return $ EOpAnd ne1 ne2
  EOpOr e1 e2 -> do
    ne1 <- mapExpr f e1
    ne2 <- mapExpr f e2
    return $ EOpOr ne1 ne2

mapLExpr :: (Monad m) => (Integer -> m Integer, String -> m String) -> LogicExpr -> m LogicExpr
mapLExpr f ex = case ex of
  LTrue -> return LTrue
  LFalse -> return LFalse
  LOpEq e1 e2 -> do
    ne1 <- mapExpr f e1
    ne2 <- mapExpr f e2
    return $ LOpEq ne1 ne2
  LOpNeq e1 e2 -> do
    ne1 <- mapExpr f e1
    ne2 <- mapExpr f e2
    return $ LOpNeq ne1 ne2
  LOpLt e1 e2 -> do
    ne1 <- mapExpr f e1
    ne2 <- mapExpr f e2
    return $ LOpLt ne1 ne2
  LOpGt e1 e2 -> do
    ne1 <- mapExpr f e1
    ne2 <- mapExpr f e2
    return $ LOpGt ne1 ne2
  LOpLe e1 e2 -> do
    ne1 <- mapExpr f e1
    ne2 <- mapExpr f e2
    return $ LOpLe ne1 ne2
  LOpGe e1 e2 -> do
    ne1 <- mapExpr f e1
    ne2 <- mapExpr f e2
    return $ LOpGe ne1 ne2