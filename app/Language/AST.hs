module Language.AST where

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
  | ECall String [Expr]
  | EIn Integer
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
  | SOut Integer Expr
  deriving (Show, Eq)
