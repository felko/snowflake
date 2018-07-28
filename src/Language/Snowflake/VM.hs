{-# LANGUAGE
    RecordWildCards
  , LambdaCase
  , TupleSections
  , TemplateHaskell
  #-}

module Language.Snowflake.VM
  ( module Language.Snowflake.VM.Types
  , runInstr
  , runVM
  , runCurrentInstr
  , runBytecode
  , bytecodeToVMState
  ) where

import Language.Snowflake.VM.Operators
import Language.Snowflake.VM.Types

import Prelude hiding (Ordering(..))

import Language.Snowflake.Parser.AST (Name)
import Language.Snowflake.Compiler.Types
import Language.Snowflake.Typing.Types

import Control.Lens hiding (uncons)
import Control.Arrow ((&&&))
import Control.Applicative ((<|>))
import Control.Monad.State
import Control.Monad.Except

import Data.Maybe (catMaybes)
import Data.Word
import Data.List
import qualified Data.Map as Map
import qualified Data.ChainMap as Env

import System.IO

debugVM :: VM ()
debugVM = get >>= \ st -> lift . lift $ do
    putStr (replicate (_vmDepth st) '\t')
    putStr $ show $ genericIndex (_vmInstrs st) (_vmInstrIndex st)
    putStr $ " | " ++ show (_vmStack st, _vmEnv st, _vmConstants st)
    getChar
    return ()

showCurrentInstr :: VM ()
showCurrentInstr = use vmDebug >>= \ dbg -> if not dbg then return () else do
  stack <- use vmStack
  constants <- use vmConstants
  symbols <- use vmSymbols
  structs <- use vmStructs
  env <- use vmEnv
  depth <- use vmDepth
  idx <- use vmInstrIndex
  instrs <- use vmInstrs
  segments <- use vmSegments
  state <- get
  lift . lift $ do
      putStrLn $ '\n' : replicate 60 '-'
      putStr (replicate depth '\t')
      putStr (show idx)
      putStr (replicate (4 - length (show idx)) ' ')
      putStr $ show (genericIndex instrs idx)
      case genericIndex instrs idx of
          STORE        addr -> putStrLn (" (" ++ show (genericIndex symbols addr) ++ ")")
          LOAD         addr -> putStrLn (" (" ++ show (genericIndex symbols addr) ++ ")")
          LOAD_CONST   addr -> putStrLn (" (" ++ show (genericIndex constants addr) ++ ")")
          BUILD_STRUCT addr -> putStrLn (" (" ++ show (genericIndex structs addr) ++ ")")
          _                 -> putStrLn ""
      putStr (showState state)
      hFlush stdout

pauseVM :: VM ()
pauseVM = lift . lift $ getChar >> return ()

applyArgs :: Segment -> [Value] -> VM Value
applyArgs (Segment c s ss i) args = do
    st@VMState{..} <- get
    let locals = Map.fromList $ catMaybes [(, val) <$> (s ^? ix i) | (i, val) <- zip [0..] args]
        state = st { _vmStack      = []
                   , _vmEnv        = Env.newChild locals _vmEnv
                   , _vmConstants  = c
                   , _vmSymbols    = s
                   , _vmStructs    = ss
                   , _vmDepth      = _vmDepth + 1
                   , _vmInstrs     = i
                   , _vmInstrIndex = 0 }
    lift (lift $ runExceptT (evalStateT runVM state)) >>= \case
        Right val -> return val
        Left  exc -> throwError exc

pop :: Word32 -> VM [Value]
pop n = do
  stack <- use vmStack
  if genericLength stack >= n then do
      (popped, rest) <- gets (splitAt (fromIntegral n) . _vmStack)
      vmStack .= rest
      return popped
  else raise StackError "pop: stack exhausted"

append :: Value -> VM ()
append val = vmStack %= (val :)

runCurrentInstr :: VM ()
runCurrentInstr = do
    instrs <- gets _vmInstrs
    debug  <- gets _vmDebug
    instrIndex <- gets (fromIntegral . _vmInstrIndex)
    case instrs ^? ix instrIndex of
        Just instr -> do
            when debug (showCurrentInstr >> pauseVM)
            runInstr instr
            vmInstrIndex += 1
        Nothing -> raise Executed ""

binOp :: (Value -> Value -> VM Value) -> VM ()
binOp op = do
    [y, x] <- pop 2
    append =<< x `op` y

unOp :: (Value -> VM Value) -> VM ()
unOp op = do
    [x] <- pop 1
    append =<< op x

runInstr :: Instr -> VM ()

runInstr NOP = return ()
runInstr POP = pop 1 >> return ()
runInstr ADD = binOp addOp
runInstr SUB = binOp subOp
runInstr MUL = binOp mulOp
runInstr DIV = binOp divOp
runInstr POW = binOp powOp
runInstr AND = binOp andOp
runInstr OR  = binOp orOp
runInstr POS = return ()
runInstr NEG = unOp negOp
runInstr NOT = unOp notOp
runInstr LT  = binOp ltOp
runInstr LE  = binOp leOp
runInstr EQ  = binOp eqOp
runInstr NEQ = binOp neqOp
runInstr GE  = binOp geOp
runInstr GT  = binOp gtOp

runInstr (CALL n) = uncons . reverse <$> pop (n + 1) >>= \case
    Just (FuncVal segIndex, args) -> do
          segments <- gets _vmSegments
          case segments ^? ix (fromIntegral segIndex) of
              Just seg -> applyArgs seg args >>= append
              Nothing  -> raise SegmentError "CALL: segment not found"
    Just (BuiltinVal f, args) -> f args >>= append
    Just (_, _) -> raise TypeError "CALL: object is not callable"
    Nothing -> raise StackError "CALL: stack exhausted"

runInstr (BUILD_LIST n) = pop n >>= append . ListVal . reverse

runInstr (BUILD_TUPLE n) = pop n >>= append . TupleVal . reverse

runInstr (BUILD_STRUCT n) = do
  structs <- gets _vmStructs
  case structs ^? ix (fromIntegral n) of
      Just fields -> append =<< (StructVal . Map.fromList . zip fields . reverse <$> pop (genericLength fields))
      Nothing     -> raise StructError "BUILD_STRUCT: struct prototype not found"

runInstr (ITER n) = head <$> pop 1 >>= \case
    ListVal [] -> runInstr (JUMP n)
    ListVal (next:rest) -> append (ListVal rest) >> append next
    _ -> raise TypeError "POP_NEXT_ITER_OR_JUMP: expected iterator"

runInstr (STORE n) = do
      symbols <- gets _vmSymbols
      case symbols ^? ix (fromIntegral n) of
          Just sym -> do
              [val] <- pop 1
              vmEnv %= Env.update sym val
          Nothing -> raise ScopeError "STORE: symbol not found"

runInstr (LOAD n) = do
    (symbols, env) <- gets (_vmSymbols &&& _vmEnv)
    case symbols ^? ix (fromIntegral n) of
        Just sym -> case Env.lookup sym env of
            Just val -> append val
            Nothing  -> raise ScopeError "LOAD: value not found"
        Nothing -> raise ScopeError "LOAD: symbol not found"

runInstr (LOAD_CONST n) = gets ((^? ix (fromIntegral n)) . _vmConstants) >>= \case
      Just cst -> append (constantToValue cst)
      Nothing -> raise SegmentError "LOAD_CONST: constant not found"

runInstr (LOAD_ATTR n) = do
    [StructVal assocs] <- pop 1
    (symbols, env) <- gets (_vmSymbols &&& _vmEnv)
    case symbols ^? ix (fromIntegral n) of
        Just sym -> append ((Map.!) assocs sym)
        Nothing -> raise AttrError "LOAD_ATTR: attribute symbol not found"

runInstr RETURN = do
    [val] <- pop 1
    raise (Returned val) ""

runInstr IF = head <$> pop 1 >>= \case
    BoolVal True  -> vmInstrIndex += 1 >> runCurrentInstr
    BoolVal False -> vmInstrIndex += 1
    _ -> raise TypeError "IF: expected boolean"

runInstr (JUMP n) = vmInstrIndex += fromIntegral n

runVM :: VM Value
runVM = (runCurrentInstr >> runVM) `catchError` \case
    VMException Executed     _ _ -> vmDepth -= 1 >> return NoneVal
    VMException (Returned x) _ _ -> vmDepth -= 1 >> return x
    err -> throwError err

runBytecode :: Bytecode -> Env -> TypeEnv -> Bool -> IO (Either VMException VMState)
runBytecode bytecode env typeEnv debug = runExceptT (execStateT runVM (bytecodeToVMState bytecode env typeEnv debug))

bytecodeToVMState :: Bytecode -> Env -> TypeEnv -> Bool -> VMState
bytecodeToVMState (Bytecode segments (Segment c s ss i) _ version) env typeEnv debug = VMState
    { _vmStack      = []
    , _vmEnv        = env
    , _vmTypeEnv    = typeEnv
    , _vmSymbols    = s
    , _vmConstants  = c
    , _vmStructs    = ss
    , _vmSegments   = segments
    , _vmDepth      = 0
    , _vmInstrs     = i
    , _vmInstrIndex = 0
    , _vmDebug      = debug
    , _vmVersion    = version }
  -- where globals = Map.fromList $ catMaybes
  --         [ (name,) <$> Env.lookup name env | (i, name) <- zip [0..] s ]
