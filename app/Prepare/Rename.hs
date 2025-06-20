module Prepare.Rename (renameTopStmts, runRenamer) where

import Control.Monad (forM, when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State (State, runState)
import Control.Monad.State.Class (get, modify, put)
import Control.Monad.Trans (lift)
import Data.Map (Map)
import Data.Map qualified as Map
import Defs (Expr, Func (..), LogicExpr, Stmt (..), TopStmt (..), TranslationError (..), TranslatorT, runTranslatorT)
import Mangle (getUniqLabel, mangleFunc, mangleGlobal)
import Tools (mapExprIdent, mapLExprIdent)

type VarNameMap = Map String String

type Renamer a = TranslatorT (State VarNameMap) a

--
rnmLabel :: String -> Renamer String
rnmLabel sym = do
  mp <- lift get
  if sym `Map.member` mp
    then
      return $ mp Map.! sym
    else do
      newSym <- getUniqLabel
      lift $ put $ Map.insert sym newSym mp
      return newSym

-- throw error when name unknown
rnmUse :: String -> Renamer String
rnmUse sym = do
  mp <- lift get
  when (sym `Map.notMember` mp) $
    throwError (TEUnknownVariable sym)
  return $ mp Map.! sym

-- rewrite name
rnmAssign :: String -> Renamer String
rnmAssign sym = do
  mp <- lift get
  newSym <- getUniqLabel
  lift $ put $ Map.insert sym newSym mp
  return newSym

-- just add global variable prefix
rnmGlob :: String -> Renamer String
rnmGlob sym = do
  let newSym = mangleGlobal sym
  lift $ modify $ Map.insert sym newSym
  return newSym

-- just add func prefix
rnmFunc :: String -> Renamer String
rnmFunc sym = do
  let newSym = mangleFunc sym
  lift $ modify $ Map.insert sym newSym
  return newSym

-- ignores name map changes
inner :: Renamer a -> Renamer a
inner func = do
  mp <- lift get
  result <- func
  lift $ put mp
  return result

runRenamer :: (Monad m) => Renamer a -> TranslatorT m (a, VarNameMap)
runRenamer rmn = do
  conf <- ask
  state <- get
  let stateM = runTranslatorT rmn state conf
  let ((mbResult, newSt, _), mp) = runState stateM Map.empty
  put newSt
  case mbResult of
    Left err -> throwError err
    Right val -> return (val, mp)

renameTopStmt :: TopStmt -> Renamer TopStmt
renameTopStmt stmt = case stmt of
  --
  TSAssign name v -> do
    nName <- rnmAssign name
    return $ TSAssign nName v
  --
  TSFunc (Func name args body) -> do
    nName <- rnmFunc name
    inner $ do
      nArgs <- forM args rnmAssign
      nBody <- renameStmts body
      return $ TSFunc (Func nName nArgs nBody)

renameStmt :: Stmt -> Renamer Stmt
renameStmt stmt = case stmt of
  --
  SAssign name ex -> do
    nEx <- renameExpr ex
    nName <- rnmAssign name
    return $ SAssign nName nEx
  --
  SMod name ex -> inner $ do
    nEx <- renameExpr ex
    nName <- rnmUse name
    return $ SMod nName nEx
  --
  SIf lx b1 mbB2 -> inner $ do
    nLx <- renameLExpr lx
    nB1 <- inner $ renameStmt b1
    nB2 <- case mbB2 of
      Nothing -> return Nothing
      Just b2 -> inner $ Just <$> renameStmt b2
    return $ SIf nLx nB1 nB2
  --
  SWhile lx b -> inner $ do
    nLx <- renameLExpr lx
    nB <- inner $ renameStmt b
    return $ SWhile nLx nB
  --
  SBlock bs -> inner $ do
    nBs <- renameStmts bs
    return $ SBlock nBs
  --
  SReturn ex -> inner $ do
    nEx <- renameExpr ex
    return $ SReturn nEx
  --
  SStore e1 e2 -> inner $ do
    nE1 <- renameExpr e1
    nE2 <- renameExpr e2
    return $ SStore nE1 nE2
  --
  SGoto l -> do
    nL <- rnmLabel l
    return $ SGoto nL
  --
  SLabel l -> do
    nL <- rnmLabel l
    return $ SLabel nL

renameTopStmts :: [TopStmt] -> Renamer [TopStmt]
renameTopStmts = mapM renameTopStmt

renameExpr :: Expr -> Renamer Expr
renameExpr = mapExprIdent rnmUse

renameLExpr :: LogicExpr -> Renamer LogicExpr
renameLExpr = mapLExprIdent rnmUse

renameStmts :: [Stmt] -> Renamer [Stmt]
renameStmts = mapM renameStmt
