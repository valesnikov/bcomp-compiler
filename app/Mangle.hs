module Mangle where

import Defs (Translator)
import Tools (getUniqId)

mangleFunction :: String -> String
mangleFunction name = "func_" ++ name

mangleConst :: Integer -> String
mangleConst num = "const_" ++ show num

mangleGlobVar :: String -> String
mangleGlobVar name = "global_" ++ name

getBranchLabel :: Translator String
getBranchLabel = do
  i <- getUniqId
  return $ "branch_" ++ show i