module Tools where

import Control.Monad (forM)
import Control.Monad.State (MonadState (get, put))
import Control.Monad.Writer.Class (tell)
import Defs (Expr (..), LogicExpr (..), TranslatorLog (..), TranslatorM, TranslatorState (..))

newTraslatorState :: TranslatorState
newTraslatorState = TranslatorState {trCounter = 0}

logMessage :: (TranslatorM m) => String -> m ()
logMessage = tell . TranslatorLog . pure

getUniqId :: (TranslatorM m) => m Integer
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
  EConst i -> EConst <$> fst f i
  EIdent s -> EIdent <$> snd f s
  ELoad ex -> ELoad <$> mapExpr f ex
  EOpNeg ex -> EOpNeg <$> mapExpr f ex
  EOpAsl ex -> EOpAsl <$> mapExpr f ex
  EOpAsr ex -> EOpAsr <$> mapExpr f ex
  EOpNot ex -> EOpNot <$> mapExpr f ex
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
  ECall name args -> do
    nName <- snd f name
    nArgs <- forM args $ mapExpr f
    return $ ECall nName nArgs

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