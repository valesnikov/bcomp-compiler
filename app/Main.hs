module Main where

import Ops (showOps)
import Optimize (postOptimize, preEvaluate)
import Parse (parseProgramm)
import Prepare (unblockVars)
import System.Environment (getArgs)
import System.IO (hPrint, stderr)
import Translate (translate)

main :: IO ()
main = do
  args <- getArgs
  str <- case args of
    inpuFile : _ -> readFile inpuFile
    _ -> getContents

  let a = showOps . postOptimize . translate . unblockVars . preEvaluate <$> parseProgramm "" str
  --let a = show <$> parseProgramm "" str

  case a of
    Left err -> hPrint stderr err
    Right result ->
      case args of
        _ : outputFile : _ -> writeFile outputFile result
        _ -> putStr result
