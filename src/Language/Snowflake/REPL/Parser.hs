module Language.Snowflake.REPL.Parser (parseREPLInput) where

import Language.Snowflake.Parser
import Language.Snowflake.REPL.Types

import Text.Parsec
import Text.Parsec.String

import Data.Tuple (swap)

replInput :: Parser REPLInput
replInput =  try (char ':' >> Command <$> replCommand <* eof)
         <|> try (Instr <$> (raw $ instruction) <* (spaces >> eof))
         <|> try (Expr <$> raw expr <* (spaces >> eof))
         <|> ((spaces <* eof) >> return NoInput)

replCommand :: Parser REPLCommand
replCommand = type' <|> load <|> reload <|> quit

type', load, reload, quit :: Parser REPLCommand
type'  = (try (string "type")   <|> string "t") <* spaces >> Type <$> raw expr
load   = (try (string "load")   <|> string "l") <* spaces >> Load <$> many anyChar
reload = (try (string "reload") <|> string "r") <* spaces >> return Reload
quit   = (try (string "quit")   <|> string "q") <* spaces >> return Quit

parseREPLInput :: String -> Either ParseError REPLInput
parseREPLInput = parse (replInput <* eof) "(input)"

raw :: Parser a -> Parser (a, String)
raw p = fmap swap $ (,) <$> lookAhead (many anyChar) <*> p
