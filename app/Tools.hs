{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Tools where

import Control.Monad.State (MonadState (get, put), evalState)
import Defs (Translator (..), TranslatorSt (..))

newTranslator :: TranslatorSt
newTranslator =
  TranslatorSt
    { counter = 0
    }

runTranslator :: Translator b -> b
runTranslator (Translator st) = evalState st newTranslator

getUniqId :: Translator Integer
getUniqId = do
  st <- get
  put $ st {counter = st.counter + 1}
  return st.counter
