module Main where

import Bcomp (showAsm)
import Control.Monad (forM_)
import Defs (TranslationConf (TranslationConf), TranslatorLog (TranslatorLog), runTranslator)
import Optimize (postOptimize, preEvaluate)
import Parse (parseProgramm)
import Prepare (renameVars)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn, stderr)
import Tools (newTraslatorState)
import Translate (translate)

main :: IO ()
main = do
  args <- getArgs
  str <- case args of
    inpuFile : _ -> readFile inpuFile
    _ -> getContents

  let mbParsed = parseProgramm "" str
  case mbParsed of
    Left err -> hPrint stderr err >> exitFailure
    Right parsed -> do
      let (mbRes, _, TranslatorLog logs) = runTranslator (translate . renameVars . preEvaluate $ parsed) newTraslatorState TranslationConf

      forM_ logs (hPutStrLn stderr)

      case showAsm . postOptimize <$> mbRes of
        Left err ->
          hPrint stderr err >> exitFailure
        Right result ->
          case args of
            _ : outputFile : _ -> writeFile outputFile result
            _ -> putStr result
