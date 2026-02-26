module Translator.Context where

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

data TranslationError
  = TENotImplemented String
  | TENumberTooBig Integer
  | TEOtherError String
  | TEUnknownVariable String
  deriving (Show, Eq)

newtype TranslatorState = TranslatorState
  {trCounter :: Integer}
  deriving (Show)

data TranslationConf = TranslationConf {}
  deriving (Show)

newtype TranslatorLog = TranslatorLog [String]
  deriving (Show, Semigroup, Monoid)

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
runTranslatorT translator state conf =
  (\((result, nextState), logs) -> (result, nextState, logs))
    <$> runWriterT (runReaderT (runStateT (runExceptT $ unTranslatorT translator) state) conf)

type Translator a = TranslatorT Identity a

runTranslator :: TranslatorT Identity result -> TranslatorState -> TranslationConf -> (Either TranslationError result, TranslatorState, TranslatorLog)
runTranslator translator state conf = runIdentity $ runTranslatorT translator state conf
