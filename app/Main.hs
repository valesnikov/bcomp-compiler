module Main where

import Bcomp (showAsm)
import Optimize (postOptimize, preEvaluate)
import Parse (parseProgramm)
import Prepare (renameVars)
import System.Environment (getArgs)
import System.IO (hPrint, stderr)
import Translate (translate)

main :: IO ()
main = do
  args <- getArgs
  str <- case args of
    inpuFile : _ -> readFile inpuFile
    _ -> getContents

  let a = showAsm . postOptimize . translate . renameVars . preEvaluate <$> parseProgramm "" str

  case a of
    Left err -> hPrint stderr err
    Right result ->
      case args of
        _ : outputFile : _ -> writeFile outputFile result
        _ -> putStr result
