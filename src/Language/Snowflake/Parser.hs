module Language.Snowflake.Parser
  ( module Exports
  , parseString
  , parseExpr
  , parseRule
  , parseFile
  , forceRight ) where

import Language.Snowflake.Parser.AST as Exports
import Language.Snowflake.Parser.Lexer as Exports
import Language.Snowflake.Parser.Rules as Exports

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Error

parseString :: String -> Either ParseError Program
parseString = parseRule (program <* eof)

parseExpr :: String -> Either ParseError Expr
parseExpr = parseRule (fmap _locNode expr <* eof)

parseRule :: Parser a -> String -> Either ParseError a
parseRule = flip parse "(string)" . (<* eof)

parseFile :: String -> IO (Either ParseError Program)
parseFile path = parse (program <* eof) path <$> readFile path

forceRight :: Show a => Either a b -> b
forceRight x = case x of
    Right res -> res
    Left  err -> error (show err)
