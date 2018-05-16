{-# LANGUAGE Rank2Types, LambdaCase, TemplateHaskell #-}

module Language.Snowflake.Compiler.CodeGen
  ( Compiler
  , compileExpr, compileInstr, compileProgram
  , execCompiler
  ) where

import Prelude hiding (Ordering(..))

import Language.Snowflake.Parser.AST
import Language.Snowflake.Compiler.Types

import Control.Lens
import Control.Applicative ((<|>))
import Control.Monad.State
import Control.Monad.Except
import Control.Arrow ((&&&))

import Data.Maybe (fromMaybe)
import Data.Word
import Data.List
import Data.Version
import Data.Time.Clock.POSIX (getPOSIXTime)

import Paths_snowflake (version)

type Compiler s a = State s a

liftTopLevel :: Compiler Segment a -> Compiler Bytecode a
liftTopLevel sc = do
    (res, newSeg) <- runState sc <$> gets _bcTopLevel
    bcTopLevel .= newSeg
    return res

sandboxBytecode :: Compiler Bytecode a -> Compiler Bytecode [Instr]
sandboxBytecode compiler = do
    bytecode <- get
    let sandboxed = execState compiler (bytecode & bcTopLevel . segInstrs .~ [])
        instrs = sandboxed ^. bcTopLevel . segInstrs
    put (sandboxed & bcTopLevel . segInstrs .~ (bytecode ^. bcTopLevel . segInstrs))
    return instrs

sandboxSegment :: Compiler Segment a -> Compiler Bytecode [Instr]
sandboxSegment compiler = do
    topLevel <- use bcTopLevel
    let sandboxed = execState compiler (topLevel & segInstrs .~ [])
        instrs = sandboxed ^. segInstrs
    bcTopLevel .= (sandboxed & segInstrs .~ (topLevel ^. segInstrs))
    return instrs
-- sandboxSegment = sandboxBytecode . liftTopLevel ?

literalToConstant :: Literal -> Constant
literalToConstant (IntLit n) = IntConst n
literalToConstant (FloatLit x) = FloatConst x
literalToConstant (BoolLit b) = BoolConst b
literalToConstant (StrLit s) = StrConst s
literalToConstant NoneLit = NoneConst

newSegment :: Segment -> Compiler Bytecode Word32
newSegment seg = do
    bcSegments %= (++ [seg])
    gets (subtract 1 . genericLength . _bcSegments)

defaultSegment = Segment
    { _segConstants = []
    , _segSymbols   = []
    , _segInstrs    = [] }

execCompiler :: Compiler Bytecode a -> IO Bytecode
execCompiler c = do
    timestamp <- round <$> getPOSIXTime
    return $ execState c (Bytecode [] defaultSegment timestamp version)

addInstr :: Instr -> Compiler Segment ()
addInstr instr = addInstrs [instr]

addInstrs :: [Instr] -> Compiler Segment ()
addInstrs instrs = segInstrs %= (++ instrs)

newID :: Eq a => Lens' Segment [a] -> a -> Compiler Segment Word32
newID lst x = do
    xs <- use lst
    case elemIndex x xs of
        Just i -> return (fromIntegral i)
        Nothing -> do
            lst %= (++ [x])
            return (genericLength xs)

newConstant :: Constant -> Compiler Segment Word32
newConstant = newID segConstants

newSymbol :: Name -> Compiler Segment Word32
newSymbol = newID segSymbols

instantiateVariable :: Name -> Compiler Segment ()
instantiateVariable var = elemIndex var <$> gets _segSymbols >>= \case
      Just i -> addInstr $ LOAD (fromIntegral i)
      Nothing -> addInstr =<< LOAD <$> newSymbol var

binOp :: BinOp -> Loc Expr -> Loc Expr -> Compiler Segment ()
binOp op a b = do
    compileExpr a
    compileExpr b
    addInstr $ case op of
        AddOp -> ADD
        SubOp -> SUB
        MulOp -> MUL
        DivOp -> DIV
        PowOp -> POW
        AndOp -> AND
        OrOp  -> OR
        LTOp  -> LT
        LEOp  -> LE
        EQOp  -> EQ
        NEQOp -> NEQ
        GEOp  -> GE
        GTOp  -> GT

unOp :: UnOp -> Loc Expr -> Compiler Segment ()
unOp op x = do
    compileExpr x
    addInstr $ case op of
        PosOp -> POS
        NegOp -> NEG
        NotOp -> NOT

compileExpr :: Loc Expr -> Compiler Segment ()
compileExpr (Loc (VarExpr name) sp) = instantiateVariable name
compileExpr (Loc (LitExpr lit) sp)  = addInstr =<< (LOAD_CONST <$> newConstant (literalToConstant (_locNode lit)))
compileExpr (Loc (BinOpExpr op a b) sp) = binOp op a b
compileExpr (Loc (UnOpExpr op x) sp) = unOp op x
compileExpr (Loc (CallExpr fn args) sp) = do
    compileExpr fn
    mapM_ compileExpr args
    addInstr $ CALL (genericLength args)
compileExpr (Loc (ListExpr lst) sp) = do
    mapM_ compileExpr lst
    addInstr $ BUILD_LIST (genericLength lst)

compileBlock :: Loc Block -> Compiler Bytecode ()
compileBlock = mapM_ compileInstr . _locNode

compileInstr :: Loc Instruction -> Compiler Bytecode ()
compileInstr (Loc (DeclareInstr _ name val) sp) = liftTopLevel $ do
    i <- newSymbol name
    compileExpr val
    addInstr (STORE i)
compileInstr (Loc (AssignInstr name val) sp) = liftTopLevel $ do
    i <- newSymbol name
    compileExpr val
    addInstr (STORE i)
compileInstr (Loc (ReturnInstr val) sp) = liftTopLevel $ compileExpr val >> addInstr RETURN
compileInstr (Loc (ExprInstr expr) sp) = liftTopLevel $ compileExpr expr >> addInstr POP
compileInstr (Loc (CondInstr cond tr fl) sp) = do
    condInstrs <- sandboxSegment (compileExpr cond)
    trInstrs <- sandboxBytecode (compileBlock tr)
    flInstrs <- sandboxBytecode (compileBlock fl)
    let trOffset   = genericLength trInstrs
        flOffset   = genericLength flInstrs
    liftTopLevel $ do
        addInstrs condInstrs
        addInstr NOT
        addInstr IF
        addInstr $ JUMP trOffset
        addInstrs trInstrs
        addInstr $ JUMP flOffset
        addInstrs flInstrs
compileInstr (Loc (ForInstr var iter loop) sp) = do
    loopInstrs <- sandboxBytecode (compileBlock loop)
    let loopOffset = genericLength loopInstrs
    liftTopLevel $ do
        compileExpr iter
        iVar  <- newSymbol var
        addInstr $ ITER (loopOffset+2)
        addInstr $ STORE iVar
        addInstrs loopInstrs
        addInstr $ JUMP (-loopOffset-3)
compileInstr (Loc (WhileInstr cond loop) sp) = do
    condInstrs <- sandboxSegment (compileExpr cond)
    loopInstrs <- sandboxBytecode (compileBlock loop)
    let condOffset = genericLength condInstrs
        loopOffset = genericLength loopInstrs
        offset     = condOffset + loopOffset
    liftTopLevel $ do
        addInstrs condInstrs
        addInstr NOT
        addInstr IF
        addInstr $ JUMP loopOffset
        addInstrs loopInstrs
        addInstr $ JUMP (-offset-4)
compileInstr (Loc (FnInstr (Loc (FnDecl name params _ body) _)) sp) = do
    bytecode <- get
    let paramSymbols = map (_paramName . _locNode) params
        initSegment  = defaultSegment & segSymbols .~ paramSymbols
        initState    = bytecode & bcTopLevel .~ initSegment
        funcBytecode = execState (compileBlock body) initState
    bcSegments .= (funcBytecode ^. bcSegments)
    segIndex <- newSegment (funcBytecode ^. bcTopLevel)
    liftTopLevel $ do
        iSymbol <- newSymbol name
        iConst  <- newConstant (FuncConst segIndex)
        addInstr $ LOAD_CONST iConst
        addInstr $ STORE iSymbol

compileProgram :: Program -> Compiler Bytecode ()
compileProgram (Program instrs) = compileBlock instrs
