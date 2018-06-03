{-# LANGUAGE
    RecordWildCards
  , LambdaCase
  #-}

module Language.Snowflake.REPL where

import Language.Snowflake.Parser
import Language.Snowflake.Typing
import Language.Snowflake.Compiler
import Language.Snowflake.REPL.Parser
import Language.Snowflake.REPL.Types
import Language.Snowflake.VM
import Language.Snowflake.Builtins

import qualified Data.ChainMap as Env

import Control.Lens
import Control.Arrow ((&&&))
import Control.Monad.State
import Control.Monad.Except

import Text.Parsec

import Data.Version

import System.Console.Haskeline

import Paths_snowflake (version)

loopREPL :: REPL ()
loopREPL = do
    fmap parseREPLInput <$> getInputLine "> " >>= \case
        Just (Right input) -> runREPLInput input `catchError` (outputStrLn . show)
        Just (Left err)    -> outputStrLn (show err)
        Nothing -> outputStrLn "Unable to parse REPL input"
    continue <- lift $ gets _replRunning
    lift $ replLine += 1
    if continue then
        loopREPL
    else
        return ()

incNodeLocLines :: IsNode n => n Loc -> Line -> n Loc
incNodeLocLines node offset = nodeUpdate node incLocLines
    where incLocLines VoidLoc = VoidLoc
          incLocLines (Loc start stop) = Loc (incSourceLine start offset) (incSourceLine stop offset)

updateNodeLineNo :: IsNode n => n Loc -> REPL (n Loc)
updateNodeLineNo node = do
    offset <- lift $ gets _replLine
    return (incNodeLocLines node offset)

bindVar :: Name -> Type -> REPL ()
bindVar name typ = lift $ replBindings %= Env.insert name typ

runREPLInput :: REPLInput -> REPL ()
runREPLInput (Instr (instr', src)) = do
    instr <- updateNodeLineNo instr'
    let modInfo = ModuleInfo src "<<instr>>"
    typeEnv <- lift . lift $ gets _vmTypeEnv
    bindings <- lift (gets _replBindings)
    case runTypeCheck instr modInfo bindings typeEnv of
        Right (cinstr, tc) -> do
            VMState{..} <- lift (lift get)
            let bc = Bytecode _vmSegments (Segment _vmConstants _vmSymbols []) 0 version
                Bytecode ss seg' _ _ = execState (compileInstr cinstr) bc
            lift . lift $ applySegment seg'
            lift $ replBindings .= _tcBindings tc
        Left errs -> printTypeCheckErrors modInfo errs
runREPLInput (Expr (expr, src)) = do
    let modInfo = ModuleInfo src "<<expr>>"
    typeEnv <- lift . lift $ gets _vmTypeEnv
    bindings <- lift (gets _replBindings)
    case runTypeCheck expr modInfo bindings typeEnv of
        Right (cexpr, tc) -> do
            VMState{..} <- lift (lift get)
            let seg  = Segment _vmConstants _vmSymbols []
                seg' = execState (compileExpr cexpr) seg
            val <- lift . lift $ applySegment seg' >> gets (head . view vmStack)
            outputStrLn (show val)
        Left errs -> printTypeCheckErrors modInfo errs
runREPLInput (Command c) = runREPLCommand c
runREPLInput NoInput = return ()

applySegment :: Segment -> VM ()
applySegment (Segment c s i) = do
    vmConstants .= c
    vmSymbols .= s
    vmInstrs .= i
    vmInstrIndex .= 0
    runVM
    vmInstrs .= []
    vmInstrIndex .= 0

printTypeCheckErrors :: ModuleInfo -> [TypeCheckError] -> REPL ()
printTypeCheckErrors modInfo errs = do
    offset <- lift (gets _replErrorOffset)
    liftIO $ printErrors modInfo offset errs

runREPLCommand :: REPLCommand -> REPL ()
runREPLCommand (Type (expr, src)) = do
    let modInfo = ModuleInfo src "<<expr>>"
    typeEnv <- lift . lift $ gets _vmTypeEnv
    bindings <- lift (gets _replBindings)
    case evalTypeCheck expr modInfo bindings typeEnv of
        Right cexpr -> outputStrLn (showType (eval cexpr))
        Left errs -> printTypeCheckErrors modInfo errs
runREPLCommand (Load path) = do
      outputStrLn $ "Loading " ++ path ++ "..."
      code <- liftIO (readFile path)
      let modInfo = ModuleInfo code path
      let mst = parse (program <* eof) path code
      st@VMState{..} <- lift (lift get)
      bindings <- lift (gets _replBindings)
      case mst of
          Right ast -> case runTypeCheck ast modInfo bindings _vmTypeEnv of
              Left errs -> printTypeCheckErrors modInfo errs
              Right (cast, tc) -> do
                  let initCompilerState = Bytecode
                          { _bcSegments  = _vmSegments
                          , _bcTopLevel  = Segment _vmConstants _vmSymbols []
                          , _bcTimestamp = 0
                          , _bcVersion   = _vmVersion }
                  let Bytecode segments (Segment c s i) _ _ = execState (compileProgram cast) initCompilerState
                  let initState = st
                          { _vmStack      = []
                          , _vmConstants  = c
                          , _vmSymbols    = s
                          , _vmSegments   = segments
                          , _vmDepth      = 0
                          , _vmInstrs     = i
                          , _vmInstrIndex = 0 }
                  eExec <- liftIO (runExceptT (runStateT runVM initState))
                  case eExec of
                      Right (val, st) -> do
                          -- outputStrLn (show val)
                          lift . lift $ put st { _vmInstrs     = []
                                               , _vmInstrIndex = 0 }
                          lift $ replBindings .= _tcBindings tc
                      Left err -> outputStrLn (show err)
          Left  err -> outputStrLn "Runtime error:" >> outputStrLn (show err)
runREPLCommand Reload = lift (gets _replFile) >>= \case
    Just f  -> runREPLCommand (Load f)
    Nothing -> outputStrLn "reload: no module loaded"
runREPLCommand Quit = lift $ replRunning .= False

runREPL :: Maybe FilePath -> Bool -> IO ()
runREPL file debug = do
    art <- readFile "src/snowflake_ascii.txt"
    putStrLn (init art ++ showVersion version)
    let loadAndLoop = case file of
            Just f  -> runREPLCommand (Load f) >> loopREPL
            Nothing -> loopREPL
    let vm = evalStateT (runInputT defaultSettings loadAndLoop) (defaultREPLState file)
    runExceptT (execStateT vm (defaultVMState debug)) >>= \case
        Right _ -> return ()
        Left err -> print err

defaultREPLState :: Maybe FilePath -> REPLState
defaultREPLState file = REPLState
    { _replFile        = file
    , _replSettings    = defaultSettings
    , _replBindings    = Env.singleMap defaultBindings
    , _replErrorOffset = 2
    , _replLine        = 0
    , _replRunning     = True }

defaultVMState :: Bool -> VMState
defaultVMState debug = VMState
    { _vmStack      = []
    , _vmEnv        = Env.singleMap defaultEnv
    , _vmTypeEnv    = Env.singleMap defaultTypeEnv
    , _vmConstants  = []
    , _vmSymbols    = []
    , _vmSegments   = []
    , _vmDepth      = 0
    , _vmInstrs     = []
    , _vmInstrIndex = 0
    , _vmDebug      = debug
    , _vmVersion    = version }
