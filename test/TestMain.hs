module Main where

import Bcomp (Addr (..), BcompAsm, CData (..), Op (..))
import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless)
import Data.Bifunctor (first)
import Language.AST (Expr (..), LogicExpr (..), Stmt (..))
import Optimize.PostOptimize (postOptimize)
import Optimize.PreEvaluate (preEvaluate)
import Parse (parseProgram)
import Prepare (renameVars)
import System.Exit (exitFailure)
import Tools (newTranslatorState)
import Translate (translate)
import Translator.Context (TranslationConf (TranslationConf), runTranslator)

data NamedTest = NamedTest String (IO ())

main :: IO ()
main = do
  results <-
    mapM
      runNamedTest
      [ NamedTest "preEvaluate: folds int16, drops dead branches and code after return" testPreEvaluateScenario,
        NamedTest "preEvaluate: keeps impure empty-if condition" testPreEvaluateImpureCondition,
        NamedTest "postOptimize: combined flow/label/store/arithmetic simplification" testPostOptimizeScenario,
        NamedTest "postOptimize: keeps START even if otherwise unused" testPostOptimizeKeepsStart,
        NamedTest "postOptimize: tracks non-absolute label addressing modes" testPostOptimizeNonAbsAddressing,
        NamedTest "pipeline: parse->optimize->translate->postOptimize end-to-end" testFullPipelineScenario
      ]

  unless (and results) exitFailure

runNamedTest :: NamedTest -> IO Bool
runNamedTest (NamedTest name testAction) = do
  testResult <- try testAction
  case testResult of
    Right () -> putStrLn $ "[PASS] " ++ name
    Left (err :: SomeException) -> do
      putStrLn $ "[FAIL] " ++ name
      putStrLn $ "       " ++ displayException err
  pure $ either (const False) (const True) testResult

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual title expected actual =
  unless (expected == actual) $
    error $
      title
        ++ "\n  expected: "
        ++ show expected
        ++ "\n  but got:  "
        ++ show actual

compileProgram :: String -> Either String BcompAsm
compileProgram source = do
  parsed <- first show $ parseProgram "<test>" source
  let prepared = renameVars $ preEvaluate parsed
      (translated, _, _) = runTranslator (translate prepared) newTranslatorState TranslationConf
  postOptimize <$> first show translated

testPreEvaluateScenario :: IO ()
testPreEvaluateScenario = do
  let input =
        SBlock
          [ SAssign "a" (EOpAdd (EConst 32767) (EConst 1)),
            SIf LFalse (SMod "a" (EConst 1)) (Just (SMod "a" (EConst 2))),
            SWhile LFalse (SMod "a" (EConst 3)),
            SReturn (EOpSub (EIdent "a") (EIdent "a")),
            SMod "a" (EConst 4)
          ]
      expected =
        SBlock
          [ SAssign "a" (EConst (-32768)),
            SMod "a" (EConst 2),
            SReturn (EConst 0)
          ]
  assertEqual "preEvaluate should collapse control-flow and keep int16 overflow semantics" expected (preEvaluate input)

testPreEvaluateImpureCondition :: IO ()
testPreEvaluateImpureCondition = do
  let input = SIf (LOpEq (EIn 1) (EIn 1)) (SBlock []) Nothing
  assertEqual "preEvaluate should not drop empty if when condition is impure" input (preEvaluate input)

testPostOptimizeScenario :: IO ()
testPostOptimizeScenario = do
  let input =
        [ OP_LABEL "START",
          OP_JUMP (AddrAbs "L1"),
          OP_ADD (AddrFwd 0),
          OP_LABEL "Ldead",
          OP_LABEL "L1",
          OP_LABEL "L2",
          OP_ST (AddrAbs "v0_x"),
          OP_ST (AddrAbs "v0_x"),
          OP_LD (AddrAbs "v0_x"),
          OP_ADD (AddrFwd 1),
          OP_SUB (AddrFwd (-1)),
          OP_BEQ "L2",
          OP_LABEL "UNUSED",
          OP_HLT
        ]
      expected =
        [ OP_LABEL "START",
          OP_JUMP (AddrAbs "Ldead"),
          OP_LABEL "Ldead",
          OP_ST (AddrAbs "v0_x"),
          OP_INC,
          OP_INC,
          OP_BEQ "Ldead",
          OP_HLT
        ]
  assertEqual "postOptimize should simplify mixed jump/label/arithmetic/store patterns" expected (postOptimize input)

testPostOptimizeKeepsStart :: IO ()
testPostOptimizeKeepsStart = do
  let input = [OP_LABEL "START", OP_LABEL "UNUSED", OP_HLT]
      expected = [OP_LABEL "START", OP_HLT]
  assertEqual "postOptimize must keep START label even when unreferenced" expected (postOptimize input)

testPostOptimizeNonAbsAddressing :: IO ()
testPostOptimizeNonAbsAddressing = do
  let input =
        [ OP_LABEL "START",
          OP_LD (AddrRel "B"),
          OP_ADD (AddrInd "C"),
          OP_OR (AddrIndI "D"),
          OP_AND (AddrIndD "E"),
          OP_LABEL "A",
          OP_LABEL "B",
          OP_LABEL "C",
          OP_LABEL "D",
          OP_LABEL "E",
          OP_HLT
        ]
      expected =
        [ OP_LABEL "START",
          OP_LD (AddrRel "A"),
          OP_ADD (AddrInd "A"),
          OP_OR (AddrIndI "A"),
          OP_AND (AddrIndD "A"),
          OP_LABEL "A",
          OP_HLT
        ]
  assertEqual "postOptimize should preserve labels referenced by non-absolute addresses" expected (postOptimize input)

testFullPipelineScenario :: IO ()
testFullPipelineScenario = do
  let source =
        unlines
          [ "a := 32767 + 1",
            "if false {",
            "  a = a + 1",
            "} else {",
            "  a = a + 0",
            "}",
            "while false {",
            "  a = 123",
            "}",
            "goto done",
            "a = a + 1",
            "done:",
            "return a - a"
          ]
      expected =
        [ OP_LABEL "c_-32768",
          OP_WORD (CWord (-32768)),
          OP_LABEL "v0_a",
          OP_WORD CWordUnd,
          OP_LABEL "START",
          OP_LD (AddrAbs "c_-32768"),
          OP_ST (AddrAbs "v0_a"),
          OP_ST (AddrAbs "v0_a"),
          OP_CLA,
          OP_HLT
        ]

  actual <- either error pure (compileProgram source)
  assertEqual "full pipeline should eliminate dead control-flow and redundant arithmetic" expected actual
