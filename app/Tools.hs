{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Tools where

import Control.Monad.State (MonadState (get, put))
import Control.Monad.State.Class (gets)
import Defs (TranslatorState (..))

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

