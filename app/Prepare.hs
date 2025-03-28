module Prepare (renameVars, resolveNames) where

import Control.Monad (forM, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Identity (Identity (runIdentity))
import Data.List (partition)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Maybe (fromJust, isJust)
import Defs (Expr (..), Func (..), LogicExpr (..), Stmt (..), TopStmt (..), TranslationError (..), Translator)
import Mangle (getUniqLabel, mangleFunction, mangleGlobVar)
import Tools (mapExprIdent, mapLExprIdent)

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

resolveNames :: [TopStmt] -> Translator [TopStmt]
resolveNames ts = do
  let (assigns, funcs) = partition isAssign ts
  newAssigns <- forM assigns $ \x -> renameTop x Map.empty
  let newMp = Map.unions $ map snd newAssigns
  newFuncs <- forM funcs $ \x -> renameTop x newMp
  return $ map fst newAssigns ++ map fst newFuncs
  where
    isAssign (TSAssign _ _) = True
    isAssign _ = False

renameTop :: TopStmt -> VarNameMap -> Translator (TopStmt, VarNameMap)
renameTop (TSAssign name val) mp = do
  let newName = mangleGlobVar name
  let newMp = Map.insert name newName mp
  return (TSAssign newName val, newMp)
renameTop (TSFunc (Func name args body)) mp = do
  let newName = mangleFunction name
  argsList <- forM args $ \arg -> do
    new <- getUniqLabel
    return (arg, new)
  let argsMp = Map.insert name newName $ Map.fromList argsList
  newBody <- renameStmts body mp
  return (TSFunc (Func name args (fst newBody)), argsMp `Map.union` mp)

renameStmts :: [Stmt] -> VarNameMap -> Translator ([Stmt], VarNameMap)
renameStmts stmts = rename stmts []
  where
    rename [] acc mp = return (reverse acc, mp)
    rename (x : xs) acc mp = do
      (nX, nMp) <- renameStmt x mp
      rename xs (nX : acc) nMp

renameStmt :: Stmt -> VarNameMap -> Translator (Stmt, VarNameMap)
renameStmt s mp = case s of
  --
  SAssign name ex -> do
    newName <- getUniqLabel
    let newMp = Map.insert name newName mp
    let newEx = renameExpr ex newMp
    return (SAssign newName newEx, newMp)
  --
  SMod name ex -> do
    checkName name mp
    let newName = mp Map.! name
    let newEx = renameExpr ex mp
    return (SMod newName newEx, mp)
  --
  SIf lexpr ifB mbElseB -> do
    let newLex = renameLExpr lexpr mp
    (newIfB, _) <- renameStmt ifB mp
    if isJust mbElseB
      then do
        (newElseB, _) <- renameStmt (fromJust mbElseB) mp
        return (SIf newLex newIfB (Just newElseB), mp)
      else do
        return (SIf newLex newIfB Nothing, mp)
  --
  SWhile lexpr stmt -> do
    let newLex = renameLExpr lexpr mp
    (newStmt, _) <- renameStmt stmt mp
    return (SWhile newLex newStmt, mp)
  --
  SBlock stmts -> do
    (newStmts, _) <- renameStmts stmts mp
    return (SBlock newStmts, mp)
  --
  SReturn ex ->
    return (SReturn (renameExpr ex mp), mp)
  --
  SStore ex1 ex2 ->
    return (SStore (renameExpr ex1 mp) (renameExpr ex2 mp), mp)
  --
  SGoto label ->
    if label `Map.member` mp
      then
        return (SGoto (mp Map.! label), mp)
      else do
        newLabel <- getUniqLabel
        let newMp = Map.insert label newLabel mp
        return (SGoto newLabel, newMp)
  --
  SLabel label ->
    if label `Map.member` mp
      then
        return (SLabel (mp Map.! label), mp)
      else do
        newLabel <- getUniqLabel
        let newMp = Map.insert label newLabel mp
        return (SLabel newLabel, newMp)

renameLExpr :: LogicExpr -> VarNameMap -> LogicExpr
renameLExpr lexpr mp =
  runIdentity $ mapLExprIdent (\x -> return $ mp Map.! x) lexpr

renameExpr :: Expr -> VarNameMap -> Expr
renameExpr ex mp =
  runIdentity $ mapExprIdent (\x -> return $ mp Map.! x) ex

checkName :: String -> VarNameMap -> Translator ()
checkName name mp = when (name `Map.notMember` mp) $ throwError $ TEUnknownVariable name
