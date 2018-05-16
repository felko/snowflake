module Language.Snowflake.Options.Types where

data Command
    = Compile CompileOptions
    | Execute ExecuteOptions
    | String  StringOptions
    | REPL    REPLOptions
    | Version

data CompileOptions = CompileOptions
    { _coFilepath     :: FilePath
    , _coOutput       :: Maybe FilePath
    , _coShowAST      :: Bool
    , _coShowBytecode :: Bool
    , _coErrorOffset  :: Int
    , _coDebug        :: Bool }

data ExecuteOptions = ExecuteOptions
    { _eoFilepath     :: FilePath
    , _eoShowAST      :: Bool
    , _eoShowBytecode :: Bool
    , _eoShowExecTime :: Bool
    , _eoErrorOffset  :: Int
    , _eoDebug        :: Bool }

data StringOptions = StringOptions
    { _soString       :: FilePath
    , _soShowAST      :: Bool
    , _soShowBytecode :: Bool
    , _soShowExecTime :: Bool
    , _soErrorOffset  :: Int
    , _soDebug        :: Bool }

data REPLOptions = REPLOptions
    { _roFile  :: Maybe FilePath
    , _roDebug :: Bool }
