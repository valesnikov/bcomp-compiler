module Mangle where

import Control.Monad.RWS (MonadState)
import Defs (TranslatorState)
import Tools (getUniqId)

mangleFunction :: String -> String
mangleFunction name = "func_" ++ name

mangleGlobVar :: String -> String
mangleGlobVar name = "glob_" ++ name

getConstName :: (Integral a, Show a) => a -> [Char]
getConstName i = "const_" ++ show i

getUniqLabel :: (MonadState TranslatorState m) => m String
getUniqLabel = do
  i <- getUniqId
  return $ 'l' : show i