{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Defs where

import Bcomp (Op)
import Control.Monad.State
  ( MonadState (get, put),
    State,
    StateT (StateT),
  )
import Data.Int (Int16)
import Data.Set (Set)

data Expr
  = EConst Integer -- literal
  | EIdent String -- variable identifier
  | ELoad Expr -- load value from address
  | EOpNeg Expr -- -ex
  | EOpAsl Expr -- <<ex
  | EOpAsr Expr -- >>ex
  | EOpNot Expr -- ~ex
  | EOpAdd Expr Expr -- (ex1 + ex2)
  | EOpSub Expr Expr -- (ex1 - ex2)
  | EOpAnd Expr Expr -- (ex1 & ex2)
  | EOpOr Expr Expr -- (ex1 | ex2)
  deriving (Show, Eq)

data LogicExpr
  = LTrue
  | LFalse
  | LOpEq Expr Expr -- (ex1 == ex2)
  | LOpNeq Expr Expr -- (ex1 != ex2)
  | LOpLt Expr Expr -- (ex1 < ex2)
  | LOpGt Expr Expr -- (ex1 > ex2)
  | LOpLe Expr Expr -- (ex1 <= ex2)
  | LOpGe Expr Expr -- (ex1 >= ex2)
  deriving (Show, Eq)

data Stmt
  = SAssign String Expr
  | SMod String Expr
  | SIf LogicExpr Stmt (Maybe Stmt)
  | SWhile LogicExpr Stmt
  | SBlock [Stmt]
  | SReturn Expr
  | SStore Expr Expr
  | SGoto String
  | SLabel String
  deriving (Show, Eq)

data LdInfo = LdInfo
  { func :: String -> String,
    goto :: String -> String,
    branch :: String -> Translator String,
    const :: String -> String,
    global :: String -> String
  }

data FuncLd = FuncLD
  { name :: String,
    constants :: Set Int16,
    globals :: Set String,
    body :: LdInfo -> [Op]
  }

data TranslatorSt = TranslatorSt
  { counter :: Integer
  }

newtype Translator a = Translator (State TranslatorSt a)
  deriving (Functor, Applicative, Monad)

instance MonadState TranslatorSt Translator where
  get :: Translator TranslatorSt
  get = Translator get
  put :: TranslatorSt -> Translator ()
  put s = Translator (put s)