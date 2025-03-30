module Defs where

import Control.Monad.Except
  ( ExceptT (..),
    MonadError,
    runExceptT,
  )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (StateT (..))
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans (MonadTrans (..))

data Expr
  = EConst Integer -- literal
  | EIdent String -- variable identifier
  | ELoad Expr -- load value from address
  | EOpNeg Expr -- -ex
  | EOpAsl Expr -- <<ex
  | EOpAsr Expr -- >>ex
  | EOpNot Expr -- ~ex
  | EOpAdd Expr Expr -- (ex1 + ex2)
  | EOpSub Expr Expr -- (ex1 - ex2)
  | EOpAnd Expr Expr -- (ex1 & ex2)
  | EOpOr Expr Expr -- (ex1 | ex2)
  deriving (Show, Eq)

data LogicExpr
  = LTrue
  | LFalse
  | LOpEq Expr Expr -- (ex1 == ex2)
  | LOpNeq Expr Expr -- (ex1 != ex2)
  | LOpLt Expr Expr -- (ex1 < ex2)
  | LOpGt Expr Expr -- (ex1 > ex2)
  | LOpLe Expr Expr -- (ex1 <= ex2)
  | LOpGe Expr Expr -- (ex1 >= ex2)
  deriving (Show, Eq)

data Stmt
  = SAssign String Expr
  | SMod String Expr
  | SIf LogicExpr Stmt (Maybe Stmt)
  | SWhile LogicExpr Stmt
  | SBlock [Stmt]
  | SReturn Expr
  | SStore Expr Expr
  | SGoto String
  | SLabel String
  deriving (Show, Eq)

data TopStmt
  = TSAssign String Integer
  | TSFunc Func

data Func = Func
  { tsfName :: String,
    tsfArgs :: [String],
    tsfBody :: [Stmt]
  }

data TranslatorState = TranslatorState
  { trCounter :: Integer,
    trLogs :: [String]
  }
  deriving (Show)

data TranslationError
  = TENotImplemented String
  | TENumberTooBig Integer
  | TEOtherError String
  | TEUnknownVariable String
  deriving (Show, Eq)

newtype TranslatorT m a = TranslatorT
  { unTranslatorT :: ExceptT TranslationError (StateT TranslatorState m) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError TranslationError,
      MonadIO,
      MonadState TranslatorState
    )

instance MonadTrans TranslatorT where
  lift :: (Monad m) => m a -> TranslatorT m a
  lift = TranslatorT . lift . lift

runTranslatorT :: (Monad m) => TranslatorT m a -> TranslatorState -> m (Either TranslationError a, TranslatorState)
runTranslatorT translator = runStateT (runExceptT $ unTranslatorT translator)

evalTranslatorT :: (Monad m) => TranslatorT m a -> TranslatorState -> m (Either TranslationError a)
evalTranslatorT translator = fmap fst . runTranslatorT translator

execTranslatorT :: (Monad m) => TranslatorT m a -> TranslatorState -> m TranslatorState
execTranslatorT translator = fmap snd . runTranslatorT translator

type Translator a = TranslatorT Identity a

runTranslator :: Translator a -> TranslatorState -> (Either TranslationError a, TranslatorState)
runTranslator translator = runIdentity . runTranslatorT translator

evalTranslator :: Translator a -> TranslatorState -> Either TranslationError a
evalTranslator translator = runIdentity . evalTranslatorT translator

execTranslator :: Translator a -> TranslatorState -> TranslatorState
execTranslator translator = runIdentity . execTranslatorT translator
