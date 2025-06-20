{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Parse
  ( parseProgramm,
  )
where

import Control.Applicative ((<|>))
import Defs (Expr (..), Func (Func), LogicExpr (..), Stmt (..))
import Text.Parsec (choice, many, optionMaybe, sepBy, string, try)
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

funcDecl :: Parser Func
funcDecl = do
  _ <- string "func"
  whiteSpace
  name <- identifier
  whiteSpace
  args <- parseTuple
  whiteSpace
  body <- blockStmt
  return $ Func name args [body]

parseTuple :: Parser [String]
parseTuple = do
  _ <- char '(' -- пропускаем открывающую скобку
  items <- elementsList -- парсим список
  _ <- char ')' -- пропускаем закрывающую скобку
  return items -- возвращаем результат

elementsList :: Parser [String]
elementsList = sepBy identifier (whiteSpace >> char ',' >> whiteSpace)

program :: Parser Stmt
program = whiteSpace >> SBlock <$> many stmt <* eof

parseProgramm :: String -> String -> Either ParseError Stmt
parseProgramm = parse program