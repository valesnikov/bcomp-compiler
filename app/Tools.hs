{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Tools where

import Control.Monad.State (MonadState (get, put))
import Defs (TranslatorState (..), TranslatorT)

newTraslatorState :: TranslatorState
newTraslatorState = TranslatorState {counter = 0, logs = []}

logMessage :: (Monad m) => String -> TranslatorT m ()
logMessage msg = do
  st <- get
  put $ st {logs = msg : logs st}

getUniqId :: (Monad m) => TranslatorT m Integer
getUniqId = do
  st <- get
  put $ st {counter = counter st + 1}
  return $ counter st
