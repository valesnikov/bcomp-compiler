module Mangle where

import Defs (TranslatorM)
import Tools (getUniqId)

prefixLocal :: String
prefixLocal = "l_"

prefixFunc :: String
prefixFunc = "f_"

prefixLiteral :: String
prefixLiteral = "c_"

prefixGlobal :: String
prefixGlobal = "g_"

mangleFunc :: String -> String
mangleFunc name = prefixFunc ++ name

mangleGlobal :: String -> String
mangleGlobal name = prefixGlobal ++ name

mangleConst :: (Integral a, Show a) => a -> [Char]
mangleConst i = prefixLiteral ++ show i

getUniqLabel :: (TranslatorM m) => m String
getUniqLabel = do
  i <- getUniqId
  return $ prefixLocal ++ show i