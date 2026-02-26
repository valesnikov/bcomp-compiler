module Main where

import Bcomp (showAsm)
import Control.Monad (forM_)
import Optimize.PostOptimize (postOptimize)
import Optimize.PreEvaluate (preEvaluate)
import Parse (parseProgram)
import Prepare (renameVars)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn, stderr)
import Tools (newTranslatorState)
import Translate (translate)
import Translator.Context (TranslationConf (TranslationConf), TranslatorLog (TranslatorLog), runTranslator)

main :: IO ()
main = do
  args <- getArgs
  str <- case args of
    inputFile : _ -> readFile inputFile
    _ -> getContents

  let mbParsed = parseProgram "" str
  case mbParsed of
    Left err -> hPrint stderr err >> exitFailure
    Right parsed -> do
      let (mbRes, _, TranslatorLog logs) = runTranslator (translate . renameVars . preEvaluate $ parsed) newTranslatorState TranslationConf

      forM_ logs (hPutStrLn stderr)

      case showAsm . postOptimize <$> mbRes of
        Left err ->
          hPrint stderr err >> exitFailure
        Right result ->
          case args of
            _ : outputFile : _ -> writeFile outputFile result
            _ -> putStr result
