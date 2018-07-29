module Language.Snowflake.Typing
    ( module Exports
    , runTypeCheck
    , evalTypeCheck
    , execTypeCheck
    ) where

import Language.Snowflake.Typing.TypeCheck as Exports
import Language.Snowflake.Typing.Types     as Exports

import Language.Snowflake.Parser.AST

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as Map
import qualified Data.ChainMap as Env

-- typeCheckProgram :: Program Loc -> ModuleInfo -> Bindings -> TypeEnv -> Either [TypeCheckError] (Program (Loc, Type))
-- typeCheckProgram (Program is) modInfo bs ts = runReader (runExceptT (evalStateT (check is) (defaultTCState bs ts))) modInfo
--
-- typeCheckInstr :: Instruction Loc -> ModuleInfo -> Bindings -> TypeEnv -> Either [TypeCheckError] (Instruction (Loc, Type))
-- typeCheckInstr instr modInfo bs ts = runReader (runExceptT (evalStateT (checkInstr instr) (defaultTCState bs ts))) modInfo
--
-- typeCheckExpr :: Expr Loc -> ModuleInfo -> Bindings -> TypeEnv -> Either [TypeCheckError] (Expr (Loc, Type))
-- typeCheckExpr expr modInfo bs ts = runReader (runExceptT (evalStateT (typeOfExpr expr) (defaultTCState bs ts))) modInfo
--
-- typeCheckTypeExpr :: TypeExpr Loc -> ModuleInfo -> TypeEnv -> Either [TypeCheckError] (TypeExpr (Loc, Type))
-- typeCheckTypeExpr tExpr modInfo ts = runReader (runExceptT (evalStateT (evaluateTypeExpr tExpr) (defaultTCState Env.empty ts))) modInfo

runTypeCheck :: TypeCheckable n => n Loc -> ModuleInfo -> Bindings Type -> TypeEnv -> Either [TypeCheckError] (n (Loc, Type), TypeCheckState)
runTypeCheck n modInfo bs ts = runReader (runExceptT (runStateT (check n) (defaultTCState bs ts))) modInfo

evalTypeCheck :: TypeCheckable n => n Loc -> ModuleInfo -> Bindings Type -> TypeEnv -> Either [TypeCheckError] (n (Loc, Type))
evalTypeCheck n modInfo bs ts = fst <$> runTypeCheck n modInfo bs ts

execTypeCheck :: TypeCheckable n => n Loc -> ModuleInfo -> Bindings Type -> TypeEnv -> Either [TypeCheckError] TypeCheckState
execTypeCheck n modInfo bs ts = snd <$> runTypeCheck n modInfo bs ts

defaultTCState :: Bindings Type -> TypeEnv -> TypeCheckState
defaultTCState bs ts = TypeCheckState
    { _tcBindings     = bs
    , _tcTypeBindings = Env.empty
    , _tcTypeEnv      = ts
    , _tcExpected     = NoneT }
