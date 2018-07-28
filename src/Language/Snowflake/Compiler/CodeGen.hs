{-# LANGUAGE Rank2Types, LambdaCase, TemplateHaskell #-}

module Language.Snowflake.Compiler.CodeGen
  ( Compiler
  , compileExpr, compileInstr, compileProgram
  , execCompiler
  ) where

import Prelude hiding (Ordering(..))

import Language.Snowflake.Parser.AST
import Language.Snowflake.Compiler.Types
import Language.Snowflake.Typing.Types

import Control.Lens
import Control.Applicative ((<|>))
import Control.Monad.State
import Control.Monad.Except
import Control.Arrow ((&&&))

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Word
import Data.List
import Data.Version
import Data.Time.Clock.POSIX (getPOSIXTime)

import Paths_snowflake (version)

type Compiler s a = State s a

nodeLoc :: Node (Loc, Type) s -> Loc
nodeLoc (Node _ (loc, _)) = loc

nodeType :: Node (Loc, Type) s -> Type
nodeType (Node _ (_, typ)) = typ

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
    , _segStructs   = []
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

newStruct :: [Name] -> Compiler Segment Word32
newStruct = newID segStructs

instantiateVariable :: Name -> Compiler Segment ()
instantiateVariable var = elemIndex var <$> gets _segSymbols >>= \case
      Just i -> addInstr $ LOAD (fromIntegral i)
      Nothing -> addInstr =<< LOAD <$> newSymbol var

binOp :: BinOp -> Expr (Loc, Type) -> Expr (Loc, Type) -> Compiler Segment ()
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

unOp :: UnOp -> Expr (Loc, Type) -> Compiler Segment ()
unOp op x = do
    compileExpr x
    addInstr $ case op of
        PosOp -> POS
        NegOp -> NEG
        NotOp -> NOT

compileExpr :: Expr (Loc, Type) -> Compiler Segment ()
compileExpr (VarExpr name) = instantiateVariable name
compileExpr (AttrExpr owner attr) = do
    i <- newSymbol attr
    compileExpr owner
    addInstr $ LOAD_ATTR i
compileExpr (LitExpr lit) = addInstr =<< (LOAD_CONST <$> newConstant (literalToConstant lit))
compileExpr (BinOpExpr op a b) = binOp op a b
compileExpr (UnOpExpr op x) = unOp op x
compileExpr (CallExpr fn args) = do
    compileExpr fn
    mapM_ compileExpr args
    addInstr $ CALL (genericLength args)
compileExpr (ListExpr lst) = do
    mapM_ compileExpr lst
    addInstr $ BUILD_LIST (genericLength lst)
compileExpr (TupleExpr t) = do
    mapM_ compileExpr t
    addInstr $ BUILD_TUPLE (genericLength t)
compileExpr (StructExpr assocs) = do
    i <- newStruct (Map.keys assocs)
    mapM_ compileExpr (Map.elems assocs)
    addInstr $ BUILD_STRUCT i

compileBlock :: Decl Block (Loc, Type) -> Compiler Bytecode ()
compileBlock (Decl (Node (Block block) _)) = mapM_ compileInstr block

compileInstr :: Decl Instruction (Loc, Type) -> Compiler Bytecode ()
compileInstr (Decl (Node (DeclareInstr _ name val) _)) = liftTopLevel $ do
    i <- newSymbol name
    compileExpr val
    addInstr (STORE i)
compileInstr (Decl (Node (AssignInstr name val) _)) = liftTopLevel $ do
    i <- newSymbol name
    compileExpr val
    addInstr (STORE i)
compileInstr (Decl (Node (ReturnInstr val) _)) = liftTopLevel $ compileExpr val >> addInstr RETURN
compileInstr (Decl (Node (ExprInstr expr) _)) = liftTopLevel $ compileExpr expr >> addInstr POP
compileInstr (Decl (Node (CondInstr cond tr fl) _)) = do
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
compileInstr (Decl (Node (ForInstr var iter loop) _)) = do
    loopInstrs <- sandboxBytecode (compileBlock loop)
    let loopOffset = genericLength loopInstrs
    liftTopLevel $ do
        compileExpr iter
        iVar  <- newSymbol var
        addInstr $ ITER (loopOffset+2)
        addInstr $ STORE iVar
        addInstrs loopInstrs
        addInstr $ JUMP (-loopOffset-3)
compileInstr (Decl (Node (WhileInstr cond loop) _)) = do
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
compileInstr (Decl (Node (FnInstr (FnDecl name params _ body)) _)) = do
    bytecode <- get
    let paramSymbols = map _paramName params
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
compileInstr (Decl (Node (TypeInstr (TypeDecl name fields)) loc)) = return ()

compileProgram :: Program (Loc, Type) -> Compiler Bytecode ()
compileProgram (Program instrs) = compileBlock instrs
