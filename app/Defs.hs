module Defs where

import Control.Monad.Except
  ( ExceptT (..),
    MonadError,
    runExceptT,
  )
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.RWS (MonadWriter)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State (StateT (..))
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Writer (WriterT (runWriterT))

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
  | ECall String [Expr]
  | EIn Integer
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
  | SOut Integer Expr
  deriving (Show, Eq)

data TopStmt
  = TSAssign String Integer
  | TSFunc Func

data Func = Func
  { tsfName :: String,
    tsfArgs :: [String],
    tsfBody :: [Stmt]
  }

newtype TranslatorState = TranslatorState
  {trCounter :: Integer}
  deriving (Show)

data TranslationConf = TranslationConf {}
  deriving (Show)

newtype TranslatorLog = TranslatorLog [String]
  deriving (Show, Semigroup, Monoid)

data TranslationError
  = TENotImplemented String
  | TENumberTooBig Integer
  | TEOtherError String
  | TEUnknownVariable String
  deriving (Show, Eq)

newtype TranslatorT m a = TranslatorT
  { unTranslatorT ::
      ExceptT TranslationError (StateT TranslatorState (ReaderT TranslationConf (WriterT TranslatorLog m))) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError TranslationError,
      MonadState TranslatorState,
      MonadReader TranslationConf,
      MonadWriter TranslatorLog
    )

class
  ( MonadError TranslationError a,
    MonadState TranslatorState a,
    MonadReader TranslationConf a,
    MonadWriter TranslatorLog a
  ) =>
  TranslatorM a

instance (Monad m) => TranslatorM (TranslatorT m)

instance MonadTrans TranslatorT where
  lift :: (Monad m) => m a -> TranslatorT m a
  lift = TranslatorT . lift . lift . lift . lift

runTranslatorT :: (Monad m) => TranslatorT m result -> TranslatorState -> TranslationConf -> m (Either TranslationError result, TranslatorState, TranslatorLog)
runTranslatorT func state conf =
  (\((a, b), c) -> (a, b, c))
    <$> runWriterT (runReaderT (runStateT (runExceptT $ unTranslatorT func) state) conf)

type Translator a = TranslatorT Identity a

runTranslator :: TranslatorT Identity result -> TranslatorState -> TranslationConf -> (Either TranslationError result, TranslatorState, TranslatorLog)
runTranslator func state cfg = runIdentity $ runTranslatorT func state cfg
