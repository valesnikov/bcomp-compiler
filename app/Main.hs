module Main where

import Bcomp (showAsm)
import Control.Monad (forM_)
import Optimize.PostOptimize (postOptimize)
import Optimize.PreEvaluate (preEvaluate)
import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    metavar,
    optional,
    progDesc,
    strArgument,
    (<**>),
  )
import Parse (parseProgram)
import Prepare (renameVars)
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn, stderr)
import Tools (newTranslatorState)
import Translate (translate)
import Translator.Context (TranslationConf (TranslationConf), TranslatorLog (TranslatorLog), runTranslator)

data CliArgs = CliArgs
  { inputFile :: Maybe FilePath,
    outputFile :: Maybe FilePath
  }

cliArgsParser :: Parser CliArgs
cliArgsParser =
  CliArgs
    <$> optional
      ( strArgument
          ( metavar "INPUT"
              <> help "Path to source file (default: stdin)"
          )
      )
    <*> optional
      ( strArgument
          ( metavar "OUTPUT"
              <> help "Path to output file (default: stdout)"
          )
      )

getCliArgs :: IO CliArgs
getCliArgs =
  execParser $
    info
      (cliArgsParser <**> helper)
      ( fullDesc
          <> progDesc "Compile source code to ITMO's bcomp-ng assembly"
      )

main :: IO ()
main = do
  args <- getCliArgs
  str <- case inputFile args of
    Just path -> readFile path
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
        Right result -> case outputFile args of
          Just path -> writeFile path result
          Nothing -> putStr result
