module Parse
  ( parseProgram,
    parseProgramm,
  )
where

import Control.Applicative ((<|>))
import Language.AST (Expr (..), LogicExpr (..), Stmt (..))
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
            "goto",
            "out",
            "in"
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

expressionParser :: Parser Expr
expressionParser = buildExpressionParser operatorTable termParser
  where
    operatorTable =
      [ [ Prefix (reservedOp "*" >> pure ELoad),
          Prefix (reservedOp "+" >> pure id),
          Prefix (reservedOp "-" >> pure EOpNeg),
          Prefix (reservedOp "~" >> pure EOpNot),
          Prefix (reservedOp "<<" >> pure EOpAsl),
          Prefix (reservedOp ">>" >> pure EOpAsr)
        ],
        [ Infix (reservedOp "+" >> pure EOpAdd) AssocLeft,
          Infix (reservedOp "-" >> pure EOpSub) AssocLeft
        ],
        [ Infix (reservedOp "|" >> pure EOpOr) AssocLeft,
          Infix (reservedOp "&" >> pure EOpAnd) AssocLeft
        ]
      ]
    termParser =
      parens expressionParser
        <|> try (reserved "in" >> (EIn <$> integer))
        <|> (EConst <$> integer)
        <|> (EIdent <$> identifier)

logicExprParser :: Parser LogicExpr
logicExprParser =
  (LTrue <$ reserved "true")
    <|> (LFalse <$ reserved "false")
    <|> try (parseBinaryLogicExpr "==" LOpEq)
    <|> try (parseBinaryLogicExpr "!=" LOpNeq)
    <|> try (parseBinaryLogicExpr "<" LOpLt)
    <|> try (parseBinaryLogicExpr ">" LOpGt)
    <|> try (parseBinaryLogicExpr "<=" LOpLe)
    <|> try (parseBinaryLogicExpr ">=" LOpGe)

parseBinaryLogicExpr :: String -> (Expr -> Expr -> LogicExpr) -> Parser LogicExpr
parseBinaryLogicExpr op mkExpr = do
  lhs <- expressionParser
  reservedOp op
  rhs <- expressionParser
  pure $ mkExpr lhs rhs

statementParser :: Parser Stmt
statementParser =
  choice $
    map
      try
      [ outputStmtParser,
        blockStmtParser,
        storeStmtParser,
        gotoStmtParser,
        returnStmtParser,
        ifStmtParser,
        whileStmtParser,
        labelStmtParser,
        assignStmtParser,
        modifyStmtParser
      ]

outputStmtParser :: Parser Stmt
outputStmtParser = do
  reserved "out"
  port <- integer
  SOut port <$> expressionParser

gotoStmtParser :: Parser Stmt
gotoStmtParser = do
  reserved "goto"
  SGoto <$> identifier

labelStmtParser :: Parser Stmt
labelStmtParser = do
  label <- identifier
  reservedOp ":"
  pure $ SLabel label

assignStmtParser :: Parser Stmt
assignStmtParser = do
  var <- identifier
  reservedOp ":="
  SAssign var <$> expressionParser

modifyStmtParser :: Parser Stmt
modifyStmtParser = do
  var <- identifier
  reservedOp "="
  SMod var <$> expressionParser

storeStmtParser :: Parser Stmt
storeStmtParser = do
  reservedOp "*"
  addr <- expressionParser
  reservedOp "="
  SStore addr <$> expressionParser

returnStmtParser :: Parser Stmt
returnStmtParser = do
  reserved "return"
  SReturn <$> expressionParser

ifStmtParser :: Parser Stmt
ifStmtParser = do
  reserved "if"
  cond <- logicExprParser
  thenBranch <- blockStmtParser
  maybeElseBranch <- optionMaybe (reserved "else" >> (blockStmtParser <|> ifStmtParser))
  pure $ SIf cond thenBranch maybeElseBranch

whileStmtParser :: Parser Stmt
whileStmtParser = do
  reserved "while"
  cond <- logicExprParser
  SWhile cond <$> blockStmtParser

blockStmtParser :: Parser Stmt
blockStmtParser = do
  _ <- char '{'
  whiteSpace
  block <- SBlock <$> many statementParser
  whiteSpace
  _ <- char '}'
  whiteSpace
  pure block

programParser :: Parser Stmt
programParser = whiteSpace >> (SBlock <$> many statementParser) <* eof

parseProgram :: String -> String -> Either ParseError Stmt
parseProgram = parse programParser

-- Backward-compatible alias with legacy typo.
parseProgramm :: String -> String -> Either ParseError Stmt
parseProgramm = parseProgram
