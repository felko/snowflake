{-# LANGUAGE
    RecordWildCards
  , TemplateHaskell
  #-}

module Language.Snowflake.VM.Types
    ( Scope, Env
    , Value(..)
    , VM
    , VMState(..), vmStack, vmEnv, vmTypeEnv, vmConstants, vmSymbols, vmSegments, vmDepth, vmInstrs, vmInstrIndex, vmDebug, vmVersion
    , VMExceptionType(..)
    , VMException(..)
    , raise
    , constantToValue
    , showState
    ) where

import Language.Snowflake.Parser (Name)
import Language.Snowflake.Compiler.Types
import Language.Snowflake.Typing.Types

import Control.Lens
import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as Map
import qualified Data.ChainMap as Env

import Data.Word
import Data.Int
import Data.List (intercalate)
import Data.Version (Version)

type Scope = Map.Map Name Value
type Env = Env.ChainMap Name Value

data Value
    = IntVal Int64
    | FloatVal Float
    | BoolVal Bool
    | StrVal String
    | ListVal [Value]
    | FuncVal Word32
    | NoneVal
    | BuiltinVal ([Value] -> VM Value)

instance Show Value where
    show (IntVal n) = show n
    show (FloatVal x) = show x
    show (BoolVal b) = show b
    show (StrVal s) = show s
    show (ListVal l) = '[' : intercalate "," (map show l) ++ "]"
    show (FuncVal segIndex) = "<<function " ++ show segIndex ++ ">>"
    show NoneVal = "None"
    show (BuiltinVal _) = "<<builtin>>"

type VM a = StateT VMState (ExceptT VMException IO) a

data VMException = VMException VMExceptionType Message VMState

instance Show VMException where
    show (VMException t msg s) = show t ++ ": " ++ msg ++ " (" ++ show s ++ ")"

data VMExceptionType
    = TypeError
    | IndexError
    | ValueError
    | StackError
    | ScopeError
    | SegmentError
    | ZeroDivisionError
    | NoEntry
    | Returned Value
    | Executed
    | KeyboardInterrupt
    deriving Show

data VMState = VMState
    { _vmStack      :: [Value]
    , _vmEnv        :: Env
    , _vmTypeEnv    :: TypeEnv
    , _vmConstants  :: [Constant]
    , _vmSymbols    :: [Name]
    , _vmSegments   :: [Segment]
    , _vmDepth      :: Int
    , _vmInstrs     :: [Instr]
    , _vmInstrIndex :: Word32
    , _vmDebug      :: Bool
    , _vmVersion    :: Version }
    deriving Show
makeLenses ''VMState

showState :: VMState -> String
showState VMState{..} =
    intercalate "\n" . map (replicate _vmDepth '\t' ++) $
        [ "stack:      " ++ show _vmStack
        , "env:        " ++ show _vmEnv
        , "constants:  " ++ show _vmConstants
        , "symbols:    " ++ show _vmSymbols
        , "instrs:     "
        ] ++ map showInstr (zip [0..] _vmInstrs)
    where showInstr (i, instr)
              | i == _vmInstrIndex = "      > " ++ show i ++ (replicate (4 - length (show i)) ' ') ++ show instr
              | otherwise          = '\t' : show i ++ (replicate (4 - length (show i)) ' ') ++ show instr
          showSeg (i, (Segment constants symbols instrs)) =
              intercalate "\n" $ ['\t' : "segment " ++ show i] ++ map ("\t\t" ++)
                  [ "constants: " ++ show constants
                  , "symbols:   " ++ show symbols
                  ] ++ map showInstr (zip [0..] instrs)

raise :: VMExceptionType -> Message -> VM a
raise t msg = throwError . VMException t msg =<< get

constantToValue :: Constant -> Value
constantToValue (IntConst n) = IntVal n
constantToValue (FloatConst x) = FloatVal x
constantToValue (BoolConst b) = BoolVal b
constantToValue (StrConst s) = StrVal s
constantToValue (FuncConst segIndex) = FuncVal segIndex
constantToValue NoneConst = NoneVal
