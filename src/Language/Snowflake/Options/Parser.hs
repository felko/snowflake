module Language.Snowflake.Options.Parser (snowflakeOptions) where

import Language.Snowflake.Options.Types

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

file :: Parser FilePath
file = argument str (metavar "FILE")

output :: Parser (Maybe FilePath)
output = optional . strOption $ long "output"
                             <> short 'o'
                             <> metavar "FILE"
                             <> help "Write output to FILE"

string :: Parser String
string = argument str (metavar "STRING")

optimize :: Parser Bool
optimize = switch $ long "optimize"
                 <> short 'O'
                 <> help "Optimize the output bytecode"

showAST, showBytecode, time, debug :: Parser Bool
showAST = switch $ long "show-ast"
                <> help "Show resulting AST"
showBytecode = switch $ long "show-bytecode"
                     <> help "Show resulting bytecode"
debug = switch $ long "debug"
              <> short 'd'
              <> help "Enable debug mode"
time = switch $ long "time"
             <> short 't'
             <> help "Show execution time"

errorOffset :: Parser Int
errorOffset = opt
    where readMaybeInt = maybeReader readMaybe
          opt = option readMaybeInt $ long "error-offset"
                                   <> value 2
                                   <> help "Sets the number of displayed lines before and after an erroneous code"

compileOptions :: Parser CompileOptions
compileOptions =  CompileOptions
              <$> file
              <*> output
              <*> showAST
              <*> showBytecode
              <*> errorOffset
              <*> debug

executeOptions :: Parser ExecuteOptions
executeOptions =  ExecuteOptions
              <$> file
              <*> showAST
              <*> showBytecode
              <*> time
              <*> errorOffset
              <*> debug

stringOptions :: Parser StringOptions
stringOptions =  StringOptions
             <$> string
             <*> showAST
             <*> showBytecode
             <*> time
             <*> errorOffset
             <*> debug

replOptions :: Parser REPLOptions
replOptions =  REPLOptions
           <$> optional file
           <*> debug

snowflakeOptions :: Parser Command
snowflakeOptions = subparser $ command "compile" (info (Compile <$> compileOptions) (progDesc "Compiles the given file"))
                            <> command "execute" (info (Execute <$> executeOptions) (progDesc "Executes a Snowflake file"))
                            <> command "string"  (info (String  <$> stringOptions)  (progDesc "Executes a piece of Snowflake code"))
                            <> command "repl"    (info (REPL    <$> replOptions)    (progDesc "Starts an interactive REPL session"))
                            <> command "version" (info (pure Version)               (progDesc "Show Snowflake version"))
