module Mangle where

import Tools (getUniqId)
import Translator.Context (TranslatorM)

localLabelPrefix :: String
localLabelPrefix = "l_"

nextUniqueLabel :: (TranslatorM m) => m String
nextUniqueLabel = do
  uniqId <- getUniqId
  pure $ localLabelPrefix ++ show uniqId
