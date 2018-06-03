{-# LANGUAGE
    MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  , LambdaCase
  , TupleSections
  #-}

module Language.Snowflake.Typing.TypeCheck
    ( evalLiteral
    , evalTypeLiteral
    ) where

import Language.Snowflake.Typing.Types

import Language.Snowflake.Parser.AST
import Language.Snowflake.VM.Types

import qualified Data.ChainMap as Env

import Prelude hiding (Ordering(..))

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Data.Functor.Foldable
import Data.Functor.Compose
import Data.Foldable (foldrM)
import qualified Data.Map as Map
import Data.List (intercalate)

instance TypeCheckable Program where
    check (Program instrs) = Program <$> check instrs

instance TypeCheckable (Decl Block) where
    check (Decl (Node (Block instrs) loc)) = do
        cinstrs <- mapM check instrs
        return . Decl $ Node (Block cinstrs) (loc, NoneT)

instance TypeCheckable (Decl Instruction) where
    check (Decl (Node (DeclareInstr te v e) loc)) = do
        (cte, t)  <- checkEval te
        (ce, t') <- checkEval e
        t'' <- intersect t t' <|> raiseTC TCMismatchError ("Couldn't declare value of expected type " ++ showType t ++ " with actual type " ++ showType t') loc
        tcBindings %= Env.insert v t''
        return . Decl $ Node (DeclareInstr cte v ce) (loc, NoneT)
    check (Decl (Node (AssignInstr v e) loc)) = do
        (_, t) <- checkEval (fromNode (Node (VarExpr_ v) loc))
        (ce, t') <- checkEval e
        intersect t t' <|> raiseTC TCMismatchError ("Couldn't assign value of expected type " ++ showType t ++ " with actual type " ++ showType t') loc
        return . Decl $ Node (AssignInstr v ce) (loc, NoneT)
    check (Decl (Node (ReturnInstr e) loc)) = do
        t  <- gets _tcExpected
        (ce, t') <- checkEval e
        intersect t t' <|> raiseTC TCMismatchError ("Couldn't return value of expected type " ++ showType t ++ " with actual type " ++ showType t') loc
        return . Decl $ Node (ReturnInstr ce) (loc, NoneT)
    check (Decl (Node (ExprInstr e) loc)) = do
        ce <- check e
        return . Decl $ Node (ExprInstr ce) (loc, NoneT)
    check (Decl (Node (CondInstr cond tr fl) loc)) = do
        (ccond, condT) <- checkEval cond
        intersect condT BoolT <|> raiseTC TCMismatchError ("Expected value of type bool, got " ++ showType condT) (astNodeData cond)
        ctr <- check tr
        cfl <- check fl
        return . Decl $ Node (CondInstr ccond ctr cfl) (loc, NoneT)
    check (Decl (Node (WhileInstr cond loop) loc)) = do
        (ccond, condT) <- checkEval cond
        intersect condT BoolT <|> raiseTC TCMismatchError ("Expected value of type bool, got " ++ showType condT) (astNodeData cond)
        cloop <- check loop
        return . Decl $ Node (WhileInstr ccond cloop) (loc, NoneT)
    check (Decl (Node (ForInstr var iter loop) loc)) = checkEval iter >>= \case
        (citer, ListT t) -> do
            tcs <- get
            cloop <- sandboxCheck loop $ tcs & tcBindings %~ Env.newChild (Map.singleton var t)
            return . Decl $ Node (ForInstr var citer cloop) (loc, NoneT)
        (citer, t) -> raiseTC TCMismatchError ("Expected iterable, got " ++ showType t) (astNodeData iter)
    check (Decl (Node (FnInstr (FnDecl name params ret body)) loc)) = do
        tcs <- get
        (cret, tret) <- checkEval ret
        (cparams, pt) <- unzip <$> mapM checkEval params
        let ps = Map.fromList $ zip (map _paramName params) pt
        cbody <- sandboxCheck body $ tcs & tcBindings %~ Env.newChild ps
                                         & tcExpected .~ tret
        tcBindings %= Env.insert name (FuncT pt tret)
        return . Decl $ Node (FnInstr (FnDecl name cparams cret cbody)) (loc, NoneT)

instance TypeCheckable Param where
    check (Param t n) = Param <$> check t <*> pure n

instance TypeCheckable (AST TypeExpr_) where
    check (VarTExpr' var loc) = gets (Env.lookup var . _tcTypeEnv) >>= \case
        Just t  -> return $ fromNode (Node (VarTExpr_ var) (loc, t))
        Nothing -> raiseTC TCUndefinedTypeError ("Type " ++ show var ++ " is undefined") loc

evalTypeLiteral :: TypeLiteral -> Type
evalTypeLiteral IntTLit = IntT
evalTypeLiteral FloatTLit = FloatT
evalTypeLiteral BoolTLit = BoolT
evalTypeLiteral StrTLit = StrT
evalTypeLiteral NoneTLit = NoneT

instance TypeCheckable (AST Expr_) where
    check (VarExpr' var loc) = gets (Env.lookup var . _tcBindings) >>= \case
        Just typ -> return . fromNode $ Node (VarExpr_ var) (loc, typ)
        Nothing  -> raiseTC TCScopeError ("Name " ++ show var ++ " is undefined") loc
    check (BinOpExpr' AddOp x y loc) = (,) <$> checkEval x <*> checkEval y >>= \case
        ((cx, AnyT), (cy, t))        -> return . fromNode $ Node (BinOpExpr_ AddOp cx cy) (loc, t)
        ((cx, t), (cy, AnyT))        -> return . fromNode $ Node (BinOpExpr_ AddOp cx cy) (loc, t)
        ((cx, IntT), (cy, IntT))     -> return . fromNode $ Node (BinOpExpr_ AddOp cx cy) (loc, IntT)
        ((cx, IntT), (cy, FloatT))   -> return . fromNode $ Node (BinOpExpr_ AddOp cx cy) (loc, FloatT)
        ((cx, FloatT), (cy, IntT))   -> return . fromNode $ Node (BinOpExpr_ AddOp cx cy) (loc, FloatT)
        ((cx, FloatT), (cy, FloatT)) -> return . fromNode $ Node (BinOpExpr_ AddOp cx cy) (loc, FloatT)
        ((cx, StrT), (cy, StrT))     -> return . fromNode $ Node (BinOpExpr_ AddOp cx cy) (loc, StrT)
        ((cx, tx), (cy, ty))         -> raiseTC TCMismatchError ("Cannot add value of type " ++ showType ty ++ " to value of type " ++ showType tx) loc
    check (BinOpExpr' SubOp x y loc) = (,) <$> checkEval x <*> checkEval y >>= \case
        ((cx, AnyT), (cy, t))        -> return . fromNode $ Node (BinOpExpr_ SubOp cx cy) (loc, t)
        ((cx, t), (cy, AnyT))        -> return . fromNode $ Node (BinOpExpr_ SubOp cx cy) (loc, t)
        ((cx, IntT), (cy, IntT))     -> return . fromNode $ Node (BinOpExpr_ SubOp cx cy) (loc, IntT)
        ((cx, IntT), (cy, FloatT))   -> return . fromNode $ Node (BinOpExpr_ SubOp cx cy) (loc, FloatT)
        ((cx, FloatT), (cy, IntT))   -> return . fromNode $ Node (BinOpExpr_ SubOp cx cy) (loc, FloatT)
        ((cx, FloatT), (cy, FloatT)) -> return . fromNode $ Node (BinOpExpr_ SubOp cx cy) (loc, FloatT)
        ((cx, tx), (cy, ty))         -> raiseTC TCMismatchError ("Cannot subtract value of type " ++ showType ty ++ " from value of type " ++ showType tx) loc
    check (BinOpExpr' MulOp x y loc) = (,) <$> checkEval x <*> checkEval y >>= \case
        ((cx, AnyT), (cy, t))        -> return . fromNode $ Node (BinOpExpr_ MulOp cx cy) (loc, t)
        ((cx, t), (cy, AnyT))        -> return . fromNode $ Node (BinOpExpr_ MulOp cx cy) (loc, t)
        ((cx, IntT), (cy, IntT))     -> return . fromNode $ Node (BinOpExpr_ MulOp cx cy) (loc, IntT)
        ((cx, IntT), (cy, FloatT))   -> return . fromNode $ Node (BinOpExpr_ MulOp cx cy) (loc, FloatT)
        ((cx, FloatT), (cy, IntT))   -> return . fromNode $ Node (BinOpExpr_ MulOp cx cy) (loc, FloatT)
        ((cx, FloatT), (cy, FloatT)) -> return . fromNode $ Node (BinOpExpr_ MulOp cx cy) (loc, FloatT)
        ((cx, IntT), (cy, StrT))     -> return . fromNode $ Node (BinOpExpr_ MulOp cx cy) (loc, StrT)
        ((cx, StrT), (cy, IntT))     -> return . fromNode $ Node (BinOpExpr_ MulOp cx cy) (loc, StrT)
        ((cx, IntT), (cy, ListT t))  -> return . fromNode $ Node (BinOpExpr_ MulOp cx cy) (loc, ListT t)
        ((cx, ListT t), (cy, IntT))  -> return . fromNode $ Node (BinOpExpr_ MulOp cx cy) (loc, ListT t)
        ((cx, tx), (cy, ty))         -> raiseTC TCMismatchError ("Cannot multiply value of type " ++ showType ty ++ " with value of type " ++ showType tx) loc
    check (BinOpExpr' DivOp x y loc) = (,) <$> checkEval x <*> checkEval y >>= \case
        ((cx, AnyT), (cy, t))        -> return . fromNode $ Node (BinOpExpr_ DivOp cx cy) (loc, t)
        ((cx, t), (cy, AnyT))        -> return . fromNode $ Node (BinOpExpr_ DivOp cx cy) (loc, t)
        ((cx, IntT), (cy, IntT))     -> return . fromNode $ Node (BinOpExpr_ DivOp cx cy) (loc, FloatT)
        ((cx, IntT), (cy, FloatT))   -> return . fromNode $ Node (BinOpExpr_ DivOp cx cy) (loc, FloatT)
        ((cx, FloatT), (cy, IntT))   -> return . fromNode $ Node (BinOpExpr_ DivOp cx cy) (loc, FloatT)
        ((cx, FloatT), (cy, FloatT)) -> return . fromNode $ Node (BinOpExpr_ DivOp cx cy) (loc, FloatT)
        ((cx, tx), (cy, ty))         -> raiseTC TCMismatchError ("Cannot divide value of type " ++ showType ty ++ " by value of type " ++ showType tx) loc
    check (BinOpExpr' PowOp x y loc) = (,) <$> checkEval x <*> checkEval y >>= \case
        ((cx, AnyT), (cy, t))        -> return . fromNode $ Node (BinOpExpr_ PowOp cx cy) (loc, t)
        ((cx, t), (cy, AnyT))        -> return . fromNode $ Node (BinOpExpr_ PowOp cx cy) (loc, t)
        ((cx, IntT), (cy, IntT))     -> return . fromNode $ Node (BinOpExpr_ PowOp cx cy) (loc, FloatT)
        ((cx, IntT), (cy, FloatT))   -> return . fromNode $ Node (BinOpExpr_ PowOp cx cy) (loc, FloatT)
        ((cx, FloatT), (cy, IntT))   -> return . fromNode $ Node (BinOpExpr_ PowOp cx cy) (loc, FloatT)
        ((cx, FloatT), (cy, FloatT)) -> return . fromNode $ Node (BinOpExpr_ PowOp cx cy) (loc, FloatT)
        ((cx, tx), (cy, ty))         -> raiseTC TCMismatchError ("Cannot exponentiate value of type " ++ showType tx ++ " to value of type " ++ showType ty) loc
    check (BinOpExpr' op x y loc)
        | op `elem` [GTOp, GEOp, LEOp, LTOp] = (,) <$> checkEval x <*> checkEval y >>= \case
            ((cx, AnyT), (cy, t))        -> return . fromNode $ Node (BinOpExpr_ op cx cy) (loc, BoolT)
            ((cx, t), (cy, AnyT))        -> return . fromNode $ Node (BinOpExpr_ op cx cy) (loc, BoolT)
            ((cx, IntT), (cy, IntT))     -> return . fromNode $ Node (BinOpExpr_ op cx cy) (loc, BoolT)
            ((cx, IntT), (cy, FloatT))   -> return . fromNode $ Node (BinOpExpr_ op cx cy) (loc, BoolT)
            ((cx, FloatT), (cy, IntT))   -> return . fromNode $ Node (BinOpExpr_ op cx cy) (loc, BoolT)
            ((cx, FloatT), (cy, FloatT)) -> return . fromNode $ Node (BinOpExpr_ op cx cy) (loc, BoolT)
            ((cx, tx), (cy, ty))         -> raiseTC TCMismatchError ("Cannot compare value of type " ++ showType tx ++ " to value of type " ++ showType ty) loc
        | op `elem` [EQOp, NEQOp] = (,) <$> checkEval x <*> checkEval y >>= \case
            ((cx, AnyT), (cy, t))        -> return . fromNode $ Node (BinOpExpr_ op cx cy) (loc, BoolT)
            ((cx, t), (cy, AnyT))        -> return . fromNode $ Node (BinOpExpr_ op cx cy) (loc, BoolT)
            ((cx, IntT), (cy, IntT))     -> return . fromNode $ Node (BinOpExpr_ op cx cy) (loc, BoolT)
            ((cx, IntT), (cy, FloatT))   -> return . fromNode $ Node (BinOpExpr_ op cx cy) (loc, BoolT)
            ((cx, FloatT), (cy, IntT))   -> return . fromNode $ Node (BinOpExpr_ op cx cy) (loc, BoolT)
            ((cx, FloatT), (cy, FloatT)) -> return . fromNode $ Node (BinOpExpr_ op cx cy) (loc, BoolT)
            ((cx, StrT), (cy, StrT))     -> return . fromNode $ Node (BinOpExpr_ op cx cy) (loc, BoolT)
            ((cx, ListT t), (cy, ListT t')) ->
                if t == t' then
                    return . fromNode $ Node (BinOpExpr_ op cx cy) (loc, BoolT)
                else
                    raiseTC TCMismatchError "Cannot compare lists of different component types" loc
            ((cx, TupleT ts), (cy, TupleT ts')) -> do
                sequence_ (zipWith intersect ts ts') <|> raiseTC TCMismatchError "Cannot compare tuples of different component types" loc
                return . fromNode $ Node (BinOpExpr_ op cx cy) (loc, BoolT)
            ((cx, NoneT), (cy, NoneT)) -> return . fromNode $ Node (BinOpExpr_ op cx cy) (loc, BoolT)
        | otherwise = raiseTC TCMismatchError "Cannot compare" loc
    check (UnOpExpr' PosOp x loc) = checkEval x >>= \case
        (cx, AnyT)   -> return . fromNode $ Node (UnOpExpr_ PosOp cx) (loc, AnyT)
        (cx, IntT)   -> return . fromNode $ Node (UnOpExpr_ PosOp cx) (loc, IntT)
        (cx, FloatT) -> return . fromNode $ Node (UnOpExpr_ PosOp cx) (loc, FloatT)
        (cx, tx)     -> raiseTC TCMismatchError ("Cannot posate value of type " ++ showType tx) loc
    check (UnOpExpr' NegOp x loc) = checkEval x >>= \case
        (cx, AnyT)   -> return . fromNode $ Node (UnOpExpr_ NegOp cx) (loc, AnyT)
        (cx, IntT)   -> return . fromNode $ Node (UnOpExpr_ NegOp cx) (loc, IntT)
        (cx, FloatT) -> return . fromNode $ Node (UnOpExpr_ NegOp cx) (loc, FloatT)
        (cx, tx)     -> raiseTC TCMismatchError ("Cannot negate value of type " ++ showType tx) loc
    check (CallExpr' f args loc) = checkEval f >>= \case
        (cf, t@(FuncT ts r)) -> do
            (cargs, ts') <- unzip <$> mapM checkEval args
            sequence_ (zipWith intersect ts ts') <|> raiseTC TCMismatchError ("Couldn't call function of type " ++ showType t ++ " with arguments (" ++ intercalate ", " (map showType ts') ++ ")") loc
            return . fromNode $ Node (CallExpr_ cf cargs) (loc, r)
        (cf, t) -> raiseTC TCMismatchError ("Expected function, got " ++ show t) loc
    check (ListExpr' xs loc) = do
        (cxs, ts) <- unzip <$> mapM checkEval xs
        t <- foldrM intersect AnyT ts <|> raiseTC TCMismatchError "Expected list to be homogenous" loc
        return . fromNode $ Node (ListExpr_ cxs) (loc, ListT t)
    check (TupleExpr' xs loc) = do
        (cxs, ts) <- unzip <$> mapM checkEval xs
        return . fromNode $ Node (TupleExpr_ cxs) (loc, TupleT ts)
    check (LitExpr' lit loc) = return . fromNode $ Node (LitExpr_ lit) (loc, evalLiteral lit)

evalLiteral :: Literal -> Type
evalLiteral (IntLit _) = IntT
evalLiteral (FloatLit _) = FloatT
evalLiteral (BoolLit _) = BoolT
evalLiteral (StrLit _) = StrT
evalLiteral NoneLit = NoneT
