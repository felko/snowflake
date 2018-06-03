{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Language.Snowflake.Options.Command
    ( runCommand
    , compile
    , execute
    ) where

import Language.Snowflake.Parser
import Language.Snowflake.Typing
import Language.Snowflake.Compiler
import Language.Snowflake.VM
import Language.Snowflake.REPL
import Language.Snowflake.Options.Types
import Language.Snowflake.Builtins

import Control.Monad (when)

import qualified Data.ChainMap as Env

import Data.Time (getCurrentTime, diffUTCTime)
import Data.Version (showVersion)

import System.FilePath.Posix

import Text.Parsec

import Paths_snowflake (version)

runCommand :: Command -> IO ()
runCommand (Compile opts) = compile opts
runCommand (Execute opts) = execute opts
runCommand (String  opts) = executeString opts
runCommand (REPL    opts) = runREPL (_roFile opts) (_roDebug opts)
runCommand Version = putStrLn (showVersion version)

execute :: ExecuteOptions -> IO ()
execute opts
    | takeExtension (_eoFilepath opts) == ".sfc" = executeBytecode opts
    | otherwise = executeFile opts

executeBytecode :: ExecuteOptions -> IO ()
executeBytecode ExecuteOptions{..} = do
    bytecode <- decodeFromFile _eoFilepath
    when _eoShowBytecode (putStrLn $ showBytecode bytecode)
    start <- getCurrentTime
    runBytecode bytecode (Env.singleMap defaultEnv) (Env.singleMap defaultTypeEnv) _eoDebug
    stop <- getCurrentTime
    when _eoShowExecTime $ putStrLn ("Execution time: " ++ show (diffUTCTime stop start))

executeFile :: ExecuteOptions -> IO ()
executeFile ExecuteOptions{..} = do
    code <- readFile _eoFilepath
    let modInfo = ModuleInfo code _eoFilepath
    let mst = parse (program <* eof) _eoFilepath code
    case mst of
        Right ast -> do
            when _eoShowAST (putStrLn $ showAST ast)
            case evalTypeCheck ast modInfo (Env.singleMap defaultBindings) (Env.singleMap defaultTypeEnv) of
                Left errs -> printErrors modInfo _eoErrorOffset errs
                Right cast -> do
                    bytecode <- execCompiler (compileProgram cast)
                    when _eoShowBytecode (putStrLn $ showBytecode bytecode)
                    start <- getCurrentTime
                    runBytecode bytecode (Env.singleMap defaultEnv) (Env.singleMap defaultTypeEnv) _eoDebug
                    stop <- getCurrentTime
                    when _eoShowExecTime $ putStrLn ("Execution time: " ++ show (diffUTCTime stop start))
                    return ()
        Left  err -> print err

executeString :: StringOptions -> IO ()
executeString StringOptions{..} = do
    let modInfo = ModuleInfo _soString "<<string>>"
    case parse (program <* eof) "<<string>>" _soString of
        Right ast -> do
            when _soShowAST (putStrLn $ showAST ast)
            case evalTypeCheck ast modInfo (Env.singleMap defaultBindings) (Env.singleMap defaultTypeEnv) of
                Left errs -> printErrors modInfo _soErrorOffset errs
                Right cast -> do
                    bytecode <- execCompiler (compileProgram cast)
                    when _soShowBytecode (putStrLn $ showBytecode bytecode)
                    start <- getCurrentTime
                    runBytecode bytecode (Env.singleMap defaultEnv) (Env.singleMap defaultTypeEnv) _soDebug
                    stop <- getCurrentTime
                    when _soShowExecTime $ putStrLn ("Execution time: " ++ show (diffUTCTime stop start))
                    return ()
        Left  err -> print err

compile :: CompileOptions -> IO ()
compile CompileOptions{..} = do
    src <- readFile _coFilepath
    let modInfo = ModuleInfo src _coFilepath
        mast = parse (program <* eof) _coFilepath src
    case mast of
        Right ast -> do
            when _coShowAST (putStrLn $ showAST ast)
            case evalTypeCheck ast modInfo (Env.singleMap defaultBindings) (Env.singleMap defaultTypeEnv) of
                Left errs -> printErrors modInfo _coErrorOffset errs
                Right cast -> do
                    bytecode <- execCompiler (compileProgram cast)
                    when _coShowBytecode (putStrLn $ showBytecode bytecode)
                    let outputPath = case _coOutput of
                          Just p  -> p
                          Nothing -> getDefaultOutputPath _coFilepath
                    encodeToFile outputPath bytecode
                    return ()
        Left  err -> print err

getDefaultOutputPath :: FilePath -> FilePath
getDefaultOutputPath = flip replaceExtension "sfc"
