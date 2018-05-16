{-# LANGUAGE DeriveFunctor #-}

module Language.Snowflake.Parser.AST
  ( Name
  , Loc(..)
  , Range
  , Span
  , ModuleInfo(..)
  , Program(..)
  , Block
  , Instruction(..)
  , FnDecl(..)
  , Param(..)
  , Expr(..)
  , TypeExpr(..)
  , TypeLiteral(..)
  , BinOp(..)
  , UnOp(..)
  , Literal(..)
  , showAST
  ) where

import Data.Semigroup
import Data.Int
import Data.List (intercalate)

import Text.Parsec (SourcePos)

type Name = String

data Loc a = Loc
    { _locNode :: a
    , _locSpan :: Span }
    deriving (Eq, Show, Functor)

type Range = (SourcePos, SourcePos)
type Span = [Range]

data ModuleInfo = ModuleInfo
    { _modSource :: String
    , _modPath   :: FilePath }
    deriving Show

newtype Program = Program (Loc Block)
    deriving Show

type Block = [Loc Instruction]

data Instruction
    = DeclareInstr (Loc TypeExpr) Name (Loc Expr)
    | AssignInstr Name (Loc Expr)
    | ReturnInstr (Loc Expr)
    | ExprInstr (Loc Expr)
    | CondInstr (Loc Expr) (Loc Block) (Loc Block)
    | WhileInstr (Loc Expr) (Loc Block)
    | ForInstr Name (Loc Expr) (Loc Block)
    | FnInstr (Loc FnDecl)
    deriving Show

data FnDecl = FnDecl Name [Loc Param] (Loc TypeExpr) (Loc Block)
    deriving Show

data Param = Param
    { _paramType :: Loc TypeExpr
    , _paramName :: Name }

instance Show Param where
    show (Param t n) = "Param " ++ paren (showLoc t) ++ " " ++ show n

data Expr
    = VarExpr Name
    | BinOpExpr BinOp (Loc Expr) (Loc Expr)
    | UnOpExpr UnOp (Loc Expr)
    | CallExpr (Loc Expr) [Loc Expr]
    | ListExpr [Loc Expr]
    | TupleExpr [Loc Expr]
    | LitExpr (Loc Literal)

instance Show Expr where
    show = showExpr

data TypeExpr
    = VarTExpr Name
    | ListTExpr (Loc TypeExpr)
    | TupleTExpr [Loc TypeExpr]
    | FnTExpr [Loc TypeExpr] (Loc TypeExpr)
    | LitTExpr (Loc TypeLiteral)

instance Show TypeExpr where
    show = showTypeExpr

data TypeLiteral
    = IntTLit
    | FloatTLit
    | BoolTLit
    | StrTLit
    | NoneTLit
    deriving (Eq, Show)

data BinOp
    = PowOp
    | MulOp
    | DivOp
    | AddOp
    | SubOp
    | AndOp
    | OrOp
    | EQOp | NEQOp
    | GTOp | GEOp
    | LTOp | LEOp
    deriving (Eq, Show)

data UnOp
    = PosOp
    | NegOp
    | NotOp
    deriving (Eq, Show)

data Literal
    = IntLit Int64
    | FloatLit Float
    | BoolLit Bool
    | StrLit String
    | NoneLit
    deriving (Eq, Show)

indent :: Int -> String -> String
indent n s = concat (replicate n "    ") ++ s

surround :: String -> String -> String -> String
surround b a s = b ++ s ++ a

paren :: String -> String
paren = surround "(" ")"

brack :: String -> String
brack = surround "[" "]"

showLoc :: Show a => Loc a -> String
showLoc (Loc x _) = show x

showAST :: Program -> String
showAST (Program instrs) = showBlock instrs

showBlock :: Loc Block -> String
showBlock block = showBlockWithIndent block 0

showBlockWithIndent :: Loc Block -> Int -> String
showBlockWithIndent (Loc instrs _) n = intercalate "\n" $ map (indent n . flip showInstrWithIndent n) instrs

showInstrWithIndent :: Loc Instruction -> Int -> String
showInstrWithIndent (Loc i _) n = indent n $ case i of
    (DeclareInstr typ name val) -> "Declare " ++ paren (showLoc typ) ++ " " ++ show name ++ " " ++ paren (showLoc val)
    (AssignInstr name val)      -> "Assign " ++ show name ++ " " ++ paren (showLoc val)
    (ReturnInstr val)           -> "Return " ++ paren (showLoc val)
    (ExprInstr expr)            -> "Expr " ++ paren (showLoc expr)
    (CondInstr cond tr fl)      -> "Cond " ++ paren (showLoc cond)
                            `endl` indent (n+1) "true:"
                            `endl` showBlockWithIndent tr (n+2)
                            `endl` indent (n+1) "false:"
                            `endl` showBlockWithIndent fl (n+2)
    (WhileInstr cond loop) -> "While " ++ paren (showLoc cond)
                       `endl` showBlockWithIndent loop (n+1)
    (ForInstr var iter loop) -> "For " ++ show var ++ " in " ++ paren (showLoc iter)
                         `endl` showBlockWithIndent loop (n+1)
    (FnInstr (Loc (FnDecl name params ret body) _)) -> "Function " ++ show name
                                                `endl` indent 1 ("args: " ++ paren (intercalate ", " (map showLoc params)))
                                                `endl` indent 1 ("returns: " ++ showLoc ret)
                                                `endl` indent 1 "body:"
                                                `endl` showBlockWithIndent body 1

showExpr :: Expr -> String
showExpr (VarExpr n) = "Var " ++ show n
showExpr (BinOpExpr op x y) = "BinOp " ++ show op ++ " " ++ paren (showLoc x) ++ " " ++ paren (showLoc y)
showExpr (UnOpExpr op x) = "UnOp " ++ show op ++ " " ++ paren (showLoc x)
showExpr (CallExpr f args) = "Call " ++ paren (showLoc f) ++ " " ++ brack (intercalate ", " (map showLoc args))
showExpr (ListExpr xs) = "List " ++ brack (intercalate ", " (map showLoc xs))
showExpr (TupleExpr xs) = "Tuple " ++ paren (intercalate ", " (map showLoc xs))
showExpr (LitExpr l) = "Lit " ++ paren (show $ _locNode l)

showTypeExpr :: TypeExpr -> String
showTypeExpr (VarTExpr n) = "Var " ++ show n
showTypeExpr (ListTExpr t) = "List " ++ brack (showLoc t)
showTypeExpr (TupleTExpr ts) = "Tuple " ++ paren (intercalate ", " (map showLoc ts))
showTypeExpr (FnTExpr ps r) = "Fn " ++ paren (intercalate ", " (map showLoc ps)) ++ " " ++ showLoc r
showTypeExpr (LitTExpr l) = "Lit " ++ show (_locNode l)


infixl 2 `endl`

endl :: String -> String -> String
s1 `endl` s2 = s1 ++ "\n" ++ s2
