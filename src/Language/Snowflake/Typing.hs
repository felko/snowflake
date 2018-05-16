module Language.Snowflake.Typing
    ( module Exports
    , typeCheckProgram
    , typeCheckInstr
    , typeCheckExpr
    , typeCheckTypeExpr
    ) where

import Language.Snowflake.Typing.TypeCheck as Exports
import Language.Snowflake.Typing.Types     as Exports

import Language.Snowflake.Parser.AST

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as Map
import qualified Data.ChainMap as Env

typeCheckProgram :: Program -> ModuleInfo -> Bindings -> TypeEnv -> Either [TypeCheckError] TypeCheckState
typeCheckProgram (Program is) modInfo bs ts = runReader (runExceptT (execStateT (checkBlock is) (defaultTCState bs ts))) modInfo

typeCheckInstr :: Loc Instruction -> ModuleInfo -> Bindings -> TypeEnv -> Either [TypeCheckError] TypeCheckState
typeCheckInstr instr modInfo bs ts = runReader (runExceptT (execStateT (checkInstr instr) (defaultTCState bs ts))) modInfo

typeCheckExpr :: Loc Expr -> ModuleInfo -> Bindings -> TypeEnv -> Either [TypeCheckError] Type
typeCheckExpr expr modInfo bs ts = runReader (runExceptT (evalStateT (typeOfExpr expr) (defaultTCState bs ts))) modInfo

typeCheckTypeExpr :: Loc TypeExpr -> ModuleInfo -> TypeEnv -> Either [TypeCheckError] Type
typeCheckTypeExpr tExpr modInfo ts = runReader (runExceptT (evalStateT (evaluateTypeExpr tExpr) (defaultTCState Env.empty ts))) modInfo

defaultTCState :: Bindings -> TypeEnv -> TypeCheckState
defaultTCState bs ts = TypeCheckState
    { _tcBindings = bs
    , _tcTypeEnv  = ts
    , _tcExpected = NoneT }
