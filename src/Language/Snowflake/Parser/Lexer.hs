module Language.Snowflake.Parser.Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef, LanguageDef)
import Text.Parsec.Token (makeTokenParser, TokenParser, GenLanguageDef(..))
import qualified Text.Parsec.Token as Token

language :: LanguageDef ()
language = emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , nestedComments  = True
    , identStart      = lower
    , identLetter     = alphaNum
    , reservedNames   =
          [ "fn", "return"
          , "if", "else", "while", "for", "in"
          , "true", "false", "none"
          , "int", "float", "bool", "str"
          , "not", "and", "or" ]
    , reservedOpNames =
          [ "->"
          , "=", ";"
          , "+", "-", "*", "/", "^"
          , "<", "<=", "==", "!=", ">=", ">" ]
    , caseSensitive   = True }

lexer :: TokenParser ()
lexer = makeTokenParser language


integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

angles :: Parser a -> Parser a
angles = Token.angles lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Token.commaSep lexer

semi :: Parser ()
semi = Token.semi lexer >> return ()

identifier :: Parser String
identifier = Token.identifier lexer <?> "identifier"

operator :: Parser String
operator = Token.operator lexer <?> "operator"

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer
