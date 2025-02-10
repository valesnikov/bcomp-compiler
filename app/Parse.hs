module Parse
  ( Expr (..),
    LogicExpr (..),
    Stmt (..),
    parseProgramm,
  )
where

import Control.Applicative ((<|>))
import Text.Parsec (choice, many, optionMaybe, try)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (eof)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Expr
  ( Assoc (AssocLeft),
    Operator (Infix, Prefix),
    buildExpressionParser,
  )
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as Token

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

lexer :: Token.TokenParser ()
lexer =
  Token.makeTokenParser
    emptyDef
      { Token.reservedNames =
          [ "if",
            "else",
            "while",
            "return",
            "true",
            "false",
            "goto"
          ],
        Token.reservedOpNames =
          [ ":",
            "*",
            "~",
            "<<",
            ">>",
            "+",
            "-",
            "|",
            "&",
            "=",
            ":=",
            "==",
            "!=",
            "<",
            ">",
            "<=",
            ">="
          ],
        Token.commentStart = "/*",
        Token.commentEnd = "*/",
        Token.commentLine = "//",
        Token.nestedComments = False,
        Token.caseSensitive = True
      }

identifier :: Parser String
identifier = Token.identifier lexer

integer :: Parser Integer
integer = Token.integer lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

-- Парсим выражения
expr :: Parser Expr
expr = buildExpressionParser table term
  where
    table =
      [ [ Prefix (reservedOp "*" >> return ELoad),
          Prefix (reservedOp "+" >> return id),
          Prefix (reservedOp "-" >> return EOpNeg),
          Prefix (reservedOp "~" >> return EOpNot),
          Prefix (reservedOp "<<" >> return EOpAsl),
          Prefix (reservedOp ">>" >> return EOpAsr)
        ],
        [ Infix (reservedOp "+" >> return EOpAdd) AssocLeft,
          Infix (reservedOp "-" >> return EOpSub) AssocLeft
        ],
        [ Infix (reservedOp "|" >> return EOpOr) AssocLeft,
          Infix (reservedOp "&" >> return EOpAnd) AssocLeft
        ]
      ]
    term =
      parens expr
        <|> (EConst <$> integer)
        <|> (EIdent <$> identifier)

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

applyM :: (Functor f) => t -> f (t -> b) -> f b
applyM val = fmap (\x -> x val)

lexpr :: Parser LogicExpr
lexpr =
  (LTrue <$ reserved "true")
    <|> (LFalse <$ reserved "false")
    <|> try (applyM LOpEq (lexprC "=="))
    <|> try (applyM LOpNeq (lexprC "!="))
    <|> try (applyM LOpLt (lexprC "<"))
    <|> try (applyM LOpGt (lexprC ">"))
    <|> try (applyM LOpLe (lexprC "<="))
    <|> try (applyM LOpGe (lexprC ">="))
  where
    lexprC op = do
      ex1 <- expr
      reservedOp op
      ex2 <- expr
      return (\f -> f ex1 ex2)

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

stmt :: Parser Stmt
stmt =
  choice $
    fmap
      try
      [ blockStmt,
        storeStmt,
        gotoStmt,
        returnStmt,
        ifStmt,
        whileStmt,
        labelStmt,
        assignStmt,
        modifyStmt
      ]

gotoStmt :: Parser Stmt
gotoStmt = do
  _ <- reserved "goto"
  SGoto <$> identifier

labelStmt :: Parser Stmt
labelStmt = do
  label <- identifier
  _ <- reservedOp ":"
  return $ SLabel label

assignStmt :: Parser Stmt
assignStmt = do
  var <- identifier
  reservedOp ":="
  SAssign var <$> expr

modifyStmt :: Parser Stmt
modifyStmt = do
  var <- identifier
  reservedOp "="
  SMod var <$> expr

storeStmt :: Parser Stmt
storeStmt = do
  _ <- reservedOp "*"
  addr <- expr
  reservedOp "="
  SStore addr <$> expr

returnStmt :: Parser Stmt
returnStmt = do
  reserved "return"
  SReturn <$> expr

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  cond <- lexpr
  ifPart <- blockStmt
  elsePart <- optionMaybe (reserved "else" >> (blockStmt <|> ifStmt))
  return (SIf cond ifPart elsePart)

whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  cond <- lexpr
  SWhile cond <$> blockStmt

blockStmt :: Parser Stmt
blockStmt = do
  _ <- char '{'
  whiteSpace
  stmts <- SBlock <$> many stmt
  whiteSpace
  _ <- char '}'
  whiteSpace
  return stmts

program :: Parser Stmt
program = whiteSpace >> SBlock <$> many stmt <* eof

parseProgramm :: String -> String -> Either ParseError Stmt
parseProgramm = parse program