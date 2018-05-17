{-# LANGUAGE LambdaCase, TupleSections #-}

module Language.Snowflake.Typing.TypeCheck
    ( checkInstr
    , checkBlock
    , evaluateTypeExpr
    , typeOfExpr
    , typeOfLit
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

import Data.Foldable (foldrM)
import qualified Data.Map as Map
import Data.List (intercalate)

checkBlock :: Loc Block -> TypeCheck ()
checkBlock = mapM_ checkInstr . _locNode

checkInstr :: Loc Instruction -> TypeCheck ()
checkInstr (Loc (DeclareInstr te v e) sp) = do
    t  <- evaluateTypeExpr te
    t' <- Loc <$> typeOfExpr e <*> pure (_locSpan e)
    t'' <- intersect (Loc t (_locSpan te)) t' <|> raiseTC TCMismatchError ("Couldn't declare value of expected type " ++ showType t ++ " with actual type " ++ showType (_locNode t')) sp
    tcBindings %= Env.insert v (_locNode t'')
checkInstr (Loc (AssignInstr v e) sp) = do
    t  <- typeOfExpr (Loc (VarExpr v) sp)
    t' <- Loc <$> typeOfExpr e <*> pure (_locSpan e)
    intersect (Loc t sp) t' <|> raiseTC TCMismatchError ("Couldn't assign value of expected type " ++ showType t ++ " with actual type " ++ showType (_locNode t')) sp
    return ()
checkInstr (Loc (ReturnInstr e) sp) = do
    t  <- gets _tcExpected
    t' <- Loc <$> typeOfExpr e <*> pure (_locSpan e)
    intersect (Loc t (_locSpan e)) t' <|> raiseTC TCMismatchError ("Couldn't return value of expected type " ++ showType t ++ " with actual type " ++ showType (_locNode t')) sp
    return ()
checkInstr (Loc (ExprInstr e) sp) = typeOfExpr e >> return ()
checkInstr (Loc (CondInstr cond tr fl) _) = do
    condT <- Loc <$> typeOfExpr cond <*> pure (_locSpan cond)
    intersect condT (Loc BoolT (_locSpan cond)) <|> raiseTC TCMismatchError ("Expected value of type bool, got " ++ showType (_locNode condT)) (_locSpan cond)
    checkBlock tr
    checkBlock fl
checkInstr (Loc (WhileInstr cond loop) _) = do
    condT <- Loc <$> typeOfExpr cond <*> pure (_locSpan cond)
    intersect condT (Loc BoolT (_locSpan cond)) <|> raiseTC TCMismatchError ("Expected value of type bool, got " ++ showType (_locNode condT)) (_locSpan cond)
    checkBlock loop
checkInstr (Loc (ForInstr var iter loop) sp) = typeOfExpr iter >>= \case
    ListT t -> do
        tcs <- get
        lift $ execStateT (checkBlock loop) (tcs & tcBindings %~ Env.newChild (Map.singleton var t))
        return ()
    t -> raiseTC TCMismatchError ("Expected iterable, got " ++ showType t) (_locSpan iter)
checkInstr (Loc (FnInstr (Loc (FnDecl name params ret body) _)) sp) = do
    tcs <- get
    t <- evaluateTypeExpr ret
    pt <- mapM (evaluateTypeExpr . _paramType . _locNode) params
    let ps = Map.fromList $ zip (map (_paramName . _locNode) params) pt
    lift $ execStateT (checkBlock body) $ tcs & tcBindings  %~ Env.newChild ps
                                              & tcExpected .~ t
    tcBindings %= Env.insert name (FuncT pt t)
    return ()

evaluateTypeExpr :: Loc TypeExpr -> TypeCheck Type
evaluateTypeExpr (Loc (VarTExpr var) sp) = gets (Env.lookup var . _tcTypeEnv) >>= \case
    Just t  -> return t
    Nothing -> raiseTC TCUndefinedTypeError ("Type " ++ show var ++ " is undefined") sp
evaluateTypeExpr (Loc (ListTExpr t) sp) = ListT <$> evaluateTypeExpr t
evaluateTypeExpr (Loc (TupleTExpr ts) sp) = TupleT <$> mapM evaluateTypeExpr ts
evaluateTypeExpr (Loc (FnTExpr ts r) sp) = FuncT <$> mapM evaluateTypeExpr ts <*> evaluateTypeExpr r
evaluateTypeExpr (Loc (LitTExpr lit) sp) = return (typeOfTypeLit lit)

typeOfTypeLit :: Loc TypeLiteral -> Type
typeOfTypeLit (Loc IntTLit _) = IntT
typeOfTypeLit (Loc FloatTLit _) = FloatT
typeOfTypeLit (Loc BoolTLit _) = BoolT
typeOfTypeLit (Loc StrTLit _) = StrT
typeOfTypeLit (Loc NoneTLit _) = NoneT

typeOfExpr :: Loc Expr -> TypeCheck Type
typeOfExpr (Loc (VarExpr var) sp) = gets (Env.lookup var . _tcBindings) >>= \case
    Just t  -> return t
    Nothing -> raiseTC TCScopeError ("Name " ++ show var ++ " is undefined") sp
typeOfExpr (Loc (BinOpExpr AddOp x y) sp) = (,) <$> typeOfExpr x <*> typeOfExpr y >>= \case
    (AnyT, t) -> return t
    (t, AnyT) -> return t
    (IntT, IntT) -> return IntT
    (IntT, FloatT) -> return FloatT
    (FloatT, IntT) -> return FloatT
    (FloatT, FloatT) -> return FloatT
    (StrT, StrT) -> return StrT
    (tx, ty) -> raiseTC TCMismatchError ("Cannot add value of type " ++ showType ty ++ " to value of type " ++ showType tx) sp
typeOfExpr (Loc (BinOpExpr SubOp x y) sp) = (,) <$> typeOfExpr x <*> typeOfExpr y >>= \case
    (AnyT, t) -> return t
    (t, AnyT) -> return t
    (IntT, IntT) -> return IntT
    (IntT, FloatT) -> return FloatT
    (FloatT, IntT) -> return FloatT
    (FloatT, FloatT) -> return FloatT
    (tx, ty) -> raiseTC TCMismatchError ("Cannot subtract value of type " ++ showType ty ++ " from value of type " ++ showType tx) sp
typeOfExpr (Loc (BinOpExpr MulOp x y) sp) = (,) <$> typeOfExpr x <*> typeOfExpr y >>= \case
    (AnyT, t) -> return t
    (t, AnyT) -> return t
    (IntT, IntT) -> return IntT
    (IntT, FloatT) -> return FloatT
    (FloatT, IntT) -> return FloatT
    (FloatT, FloatT) -> return FloatT
    (IntT, StrT) -> return StrT
    (StrT, IntT) -> return StrT
    (IntT, ListT t) -> return (ListT t)
    (ListT t, IntT) -> return (ListT t)
    (tx, ty) -> raiseTC TCMismatchError ("Cannot multiply value of type " ++ showType ty ++ " with value of type " ++ showType tx) sp
typeOfExpr (Loc (BinOpExpr DivOp x y) sp) = (,) <$> typeOfExpr x <*> typeOfExpr y >>= \case
    (AnyT, t) -> return t
    (t, AnyT) -> return t
    (IntT, IntT) -> return FloatT
    (IntT, FloatT) -> return FloatT
    (FloatT, IntT) -> return FloatT
    (FloatT, FloatT) -> return FloatT
    (tx, ty) -> raiseTC TCMismatchError ("Cannot divide value of type " ++ showType ty ++ " by value of type " ++ showType tx) sp
typeOfExpr (Loc (BinOpExpr PowOp x y) sp) = (,) <$> typeOfExpr x <*> typeOfExpr y >>= \case
    (AnyT, t) -> return t
    (t, AnyT) -> return t
    (IntT, IntT) -> return IntT
    (IntT, FloatT) -> return FloatT
    (FloatT, IntT) -> return FloatT
    (FloatT, FloatT) -> return FloatT
    (tx, ty) -> raiseTC TCMismatchError ("Cannot exponentiate value of type " ++ showType tx ++ " to value of type " ++ showType ty) sp
typeOfExpr (Loc (BinOpExpr op x y) sp)
    | op `elem` [GTOp, GEOp, LEOp, LTOp] = (,) <$> typeOfExpr x <*> typeOfExpr y >>= \case
        (AnyT, t) -> return BoolT
        (t, AnyT) -> return BoolT
        (IntT, IntT) -> return BoolT
        (IntT, FloatT) -> return BoolT
        (FloatT, IntT) -> return BoolT
        (FloatT, FloatT) -> return BoolT
        (tx, ty) -> raiseTC TCMismatchError ("Cannot compare value of type " ++ showType tx ++ " to value of type " ++ showType ty) sp
    | op `elem` [EQOp, NEQOp] = (,) <$> typeOfExpr x <*> typeOfExpr y >>= \case
        (AnyT, t) -> return BoolT
        (t, AnyT) -> return BoolT
        (IntT, IntT) -> return BoolT
        (IntT, FloatT) -> return BoolT
        (FloatT, IntT) -> return BoolT
        (FloatT, FloatT) -> return BoolT
        (StrT, StrT) -> return BoolT
        (ListT t, ListT t') ->
            if t == t' then
                return BoolT
            else
                raiseTC TCMismatchError "Cannot compare lists of different component types" sp
        (TupleT ts, TupleT ts') -> do
            sequence_ (zipWith (\ t t' -> intersect (Loc t sp) (Loc t' sp)) ts ts') <|> raiseTC TCMismatchError "Cannot compare tuples of different component types" sp
            return BoolT
        (NoneT, NoneT) -> return BoolT
    | otherwise = raiseTC TCMismatchError "Cannot compare" sp
typeOfExpr (Loc (UnOpExpr PosOp x) sp) = typeOfExpr x >>= \case
    AnyT -> return AnyT
    IntT -> return IntT
    FloatT -> return FloatT
    tx -> raiseTC TCMismatchError ("Cannot negate value of type " ++ showType tx) sp
typeOfExpr (Loc (UnOpExpr NegOp x) sp) = typeOfExpr x >>= \case
    AnyT -> return AnyT
    IntT -> return IntT
    FloatT -> return FloatT
    tx -> raiseTC TCMismatchError ("Cannot get positive value of type " ++ showType tx) sp
typeOfExpr (Loc (CallExpr f args) sp) = typeOfExpr f >>= \case
    t@(FuncT ts r) -> do
        ts' <- mapM (\ arg@(Loc _ sp) -> Loc <$> typeOfExpr arg <*> pure sp) args
        sequence_ (zipWith (\ t t'@(Loc _ sp') -> intersect (Loc t sp') t') ts ts') <|> raiseTC TCMismatchError ("Couldn't call function of type " ++ showType t ++ " with arguments (" ++ intercalate ", " (map (showType . _locNode) ts') ++ ")") sp
        return r
    t -> raiseTC TCMismatchError ("Expected function, got " ++ show t) sp
typeOfExpr (Loc (ListExpr xs) sp) = do
    ts <- mapM (\ x@(Loc _ sp) -> Loc <$> typeOfExpr x <*> pure sp) xs
    Loc t _ <- foldrM intersect (Loc AnyT sp) ts <|> raiseTC TCMismatchError "Expected list to be homogenous" sp
    return (ListT t)
typeOfExpr (Loc (TupleExpr xs) _) = TupleT <$> mapM typeOfExpr xs
typeOfExpr (Loc (LitExpr lit) _) = return (typeOfLit lit)

typeOfLit :: Loc Literal -> Type
typeOfLit (Loc (IntLit _) _) = IntT
typeOfLit (Loc (FloatLit _) _) = FloatT
typeOfLit (Loc (BoolLit _) _) = BoolT
typeOfLit (Loc (StrLit _) _) = StrT
typeOfLit (Loc NoneLit _) = NoneT
