{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Tools where

import Control.Monad.State (MonadState (get, put))
import Control.Monad.State.Class (gets)
import Defs (TranslatorState (..))

newTraslatorState :: TranslatorState
newTraslatorState = TranslatorState {counter = 0, logs = []}

logMessage :: (MonadState TranslatorState m) => String -> m ()
logMessage msg = do
  st <- get
  put $ st {logs = msg : logs st}

getLogs :: (MonadState TranslatorState m) => m [String]
getLogs = gets logs

getUniqId :: (MonadState TranslatorState m) => m Integer
getUniqId = do
  st <- get
  put $ st {counter = counter st + 1}
  return $ counter st
