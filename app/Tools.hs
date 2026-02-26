module Tools where

import Control.Monad.State (MonadState (get, put))
import Translator.Context (TranslatorM, TranslatorState (..))

newTranslatorState :: TranslatorState
newTranslatorState = TranslatorState {trCounter = 0}

-- Backward-compatible alias with legacy typo.
newTraslatorState :: TranslatorState
newTraslatorState = newTranslatorState

getUniqId :: (TranslatorM m) => m Integer
getUniqId = do
  st <- get
  let uniqId = trCounter st
  put $ st {trCounter = uniqId + 1}
  pure uniqId
