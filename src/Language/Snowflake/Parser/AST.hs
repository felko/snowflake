{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , TypeSynonymInstances
  , PatternSynonyms
  , ViewPatterns
  , Rank2Types
  , TemplateHaskell
  , FlexibleInstances
  #-}

module Language.Snowflake.Parser.AST
  ( AST(..)
  , Name
  , IsNode(..), nodeUpdate
  , Node(..)
  , Decl(..)
  , Loc(..)
  , ModuleInfo(..)
  , Program(..)
  , Block(..)
  , Instruction(..)
  , FnDecl(..)
  , Param(..)
  , Expr_(..)
  , Expr
  , TypeExpr_(..)
  , TypeExpr
  , TypeLiteral(..)
  , BinOp(..)
  , UnOp(..)
  , Literal(..)
  , showAST, astNodeData
  , terminal, terminalVoid, fromNode
  , pattern VarExpr', pattern BinOpExpr', pattern UnOpExpr', pattern CallExpr', pattern ListExpr', pattern TupleExpr', pattern LitExpr'
  , pattern VarExpr, pattern BinOpExpr, pattern UnOpExpr, pattern CallExpr, pattern ListExpr, pattern TupleExpr, pattern LitExpr
  , pattern VarTExpr', pattern ListTExpr', pattern TupleTExpr', pattern FnTExpr', pattern LitTExpr'
  , pattern VarTExpr, pattern ListTExpr, pattern TupleTExpr, pattern FnTExpr, pattern LitTExpr
  ) where

import Data.Functor.Foldable
import Data.Functor.Compose
import Data.Functor.Classes

import Data.Semigroup
import Data.Int
import Data.List (intercalate)

import Text.Parsec (SourcePos)

type Name = String

data Loc
    = Loc !SourcePos !SourcePos
    | VoidLoc
    deriving Show

instance Monoid Loc where
    mempty = VoidLoc
    VoidLoc `mappend` loc = loc
    loc `mappend` VoidLoc = loc
    Loc b e `mappend` Loc b' e' = Loc b'' e''
        where b'' = min b b'
              e'' = max e e'

instance Semigroup Loc where
    (<>) = mappend

class IsNode n where
    nodeData :: n s -> s
    nodeSet :: n s -> s -> n s

nodeUpdate :: IsNode n => n s -> (s -> s) -> n s
nodeUpdate node f = nodeSet node (f (nodeData node))

instance IsNode (AST n) where
    nodeData (AST (Fix (Compose (Node _ s)))) = s
    nodeSet (AST (Fix (Compose (Node n _)))) s = AST (Fix (Compose (Node n s)))

instance IsNode (Decl n) where
    nodeData (Decl (Node _ s)) = s
    nodeSet (Decl (Node n _)) s = Decl (Node n s)

instance IsNode Program where
    nodeData (Program d) = nodeData d
    nodeSet (Program d) s = Program (nodeSet d s)

instance IsNode Param where
    nodeData (Param t n) = nodeData t
    nodeSet (Param t n) s = Param (nodeSet t s) n

data Node s a = Node
    { _nodeValue :: a
    , _nodeData  :: s }
    deriving (Show, Eq, Functor)

newtype AST n s = AST { unAST :: Fix (Compose (Node s) n) }

astNodeData :: AST n s -> s
astNodeData (AST (Fix (Compose (Node _ s)))) = s

terminal :: Functor n => (forall b. a -> n b) -> Node Loc a -> AST n Loc
terminal constr x = fromNode (constr <$> x)

terminalVoid :: Functor n => (forall b. a -> n b) -> a -> AST n Loc
terminalVoid constr x = fromNode (Node (constr x) VoidLoc)

fromNode :: Functor n => Node s (n (AST n s)) -> AST n s
fromNode (Node n s) = AST . Fix . Compose $ Node (fmap unAST n) s

data ModuleInfo = ModuleInfo
    { _modSource :: String
    , _modPath   :: FilePath }
    deriving Show

newtype Program s = Program (Decl Block s)

newtype Block s = Block [Decl Instruction s]

newtype Decl n s = Decl { unDecl :: Node s (n s) }

data Instruction s
    = DeclareInstr (TypeExpr s) Name (Expr s)
    | AssignInstr Name (Expr s)
    | ReturnInstr (Expr s)
    | ExprInstr (Expr s)
    | CondInstr (Expr s) (Decl Block s) (Decl Block s)
    | WhileInstr (Expr s) (Decl Block s)
    | ForInstr Name (Expr s) (Decl Block s)
    | FnInstr (FnDecl s)

data FnDecl s = FnDecl Name [Param s] (TypeExpr s) (Decl Block s)

data Param s = Param
    { _paramType :: TypeExpr s
    , _paramName :: Name }

instance Show (Param s) where
    show (Param t n) = "Param " ++ paren (show t) ++ " " ++ show n

data Expr_ expr
    = VarExpr_ String
    | BinOpExpr_ BinOp expr expr
    | UnOpExpr_ UnOp expr
    | CallExpr_ expr [expr]
    | ListExpr_ [expr]
    | TupleExpr_ [expr]
    | LitExpr_ Literal
    deriving (Functor, Foldable, Traversable)

type Expr s = AST Expr_ s

-- State patterns
pattern VarExpr' :: String -> s -> Expr s
pattern VarExpr' v s <- AST (Fix (Compose (Node (VarExpr_ v) s)))

pattern BinOpExpr' :: BinOp -> Expr s -> Expr s -> s -> Expr s
pattern BinOpExpr' op x y s <- AST (Fix (Compose (Node (BinOpExpr_ op (AST -> x) (AST -> y)) s)))

pattern UnOpExpr' :: UnOp -> Expr s -> s -> Expr s
pattern UnOpExpr' op x s <- AST (Fix (Compose (Node (UnOpExpr_ op (AST -> x)) s)))

pattern CallExpr' :: Expr s -> [Expr s] -> s -> Expr s
pattern CallExpr' f xs s <- AST (Fix (Compose (Node (CallExpr_ (AST -> f) (map AST -> xs)) s)))

pattern ListExpr' :: [Expr s] -> s -> Expr s
pattern ListExpr' xs s <- AST (Fix (Compose (Node (ListExpr_ (map AST -> xs)) s)))

pattern TupleExpr' :: [Expr s] -> s -> Expr s
pattern TupleExpr' xs s <- AST (Fix (Compose (Node (TupleExpr_ (map AST -> xs)) s)))

pattern LitExpr' :: Literal -> s -> Expr s
pattern LitExpr' lit s <- AST (Fix (Compose (Node (LitExpr_ lit) s)))

-- Pure patterns
pattern VarExpr :: String -> Expr s
pattern VarExpr v <- VarExpr' v _

pattern BinOpExpr :: BinOp -> Expr s -> Expr s -> Expr s
pattern BinOpExpr op x y <- BinOpExpr' op x y _

pattern UnOpExpr :: UnOp -> Expr s -> Expr s
pattern UnOpExpr op x <- UnOpExpr' op x _

pattern CallExpr :: Expr s -> [Expr s] -> Expr s
pattern CallExpr f xs <- CallExpr' f xs _

pattern ListExpr :: [Expr s] -> Expr s
pattern ListExpr xs <- ListExpr' xs _

pattern TupleExpr :: [Expr s] -> Expr s
pattern TupleExpr xs <- TupleExpr' xs _

pattern LitExpr :: Literal -> Expr s
pattern LitExpr lit <- LitExpr' lit _

instance {-# OVERLAPS #-} Show (Expr s) where
    show = showExpr

data TypeExpr_ expr
    = VarTExpr_ Name
    | ListTExpr_ expr
    | TupleTExpr_ [expr]
    | FnTExpr_  [expr] expr
    | LitTExpr_ TypeLiteral
    deriving (Functor, Foldable, Traversable)

type TypeExpr s = AST TypeExpr_ s

-- State patterns
pattern VarTExpr' :: String -> s -> TypeExpr s
pattern VarTExpr' v s <- AST (Fix (Compose (Node (VarTExpr_ v) s)))

pattern ListTExpr' :: TypeExpr s -> s -> TypeExpr s
pattern ListTExpr' x s <- AST (Fix (Compose (Node (ListTExpr_ (AST -> x)) s)))

pattern TupleTExpr' :: [TypeExpr s] -> s -> TypeExpr s
pattern TupleTExpr' xs s <- AST (Fix (Compose (Node (TupleTExpr_ (map AST -> xs)) s)))

pattern FnTExpr' :: [TypeExpr s] -> TypeExpr s -> s -> TypeExpr s
pattern FnTExpr' xs ret s <- AST (Fix (Compose (Node (FnTExpr_ (map AST -> xs) (AST -> ret)) s)))

pattern LitTExpr' :: TypeLiteral -> s -> TypeExpr s
pattern LitTExpr' lit s <- AST (Fix (Compose (Node (LitTExpr_ lit) s)))

-- Pure patterns
pattern VarTExpr :: String -> TypeExpr s
pattern VarTExpr v <- VarTExpr' v _

pattern ListTExpr :: TypeExpr s -> TypeExpr s
pattern ListTExpr x <- ListTExpr' x _

pattern TupleTExpr :: [TypeExpr s] -> TypeExpr s
pattern TupleTExpr xs <- TupleTExpr' xs _

pattern FnTExpr :: [TypeExpr s] -> TypeExpr s -> TypeExpr s
pattern FnTExpr xs ret <- FnTExpr' xs ret _

pattern LitTExpr :: TypeLiteral -> TypeExpr s
pattern LitTExpr lit <- LitTExpr' lit _

instance {-# OVERLAPS #-} Show (TypeExpr s) where
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

showAST :: Program s -> String
showAST (Program instrs) = showBlock instrs

showBlock :: Decl Block s -> String
showBlock block = showBlockWithIndent block 0

showBlockWithIndent :: Decl Block s -> Int -> String
showBlockWithIndent (Decl (Node (Block block) _)) n = intercalate "\n" $ map (indent n . flip showInstrWithIndent n . _nodeValue . unDecl) block

showInstrWithIndent :: Instruction s -> Int -> String
showInstrWithIndent i n = indent n $ case i of
    (DeclareInstr typ name val) -> "Declare " ++ paren (show typ) ++ " " ++ show name ++ " " ++ paren (show val)
    (AssignInstr name val)      -> "Assign " ++ show name ++ " " ++ paren (show val)
    (ReturnInstr val)           -> "Return " ++ paren (show val)
    (ExprInstr expr)            -> "Expr " ++ paren (show expr)
    (CondInstr cond tr fl)      -> "Cond " ++ paren (show cond)
                            `endl` indent (n+1) "true:"
                            `endl` showBlockWithIndent tr (n+2)
                            `endl` indent (n+1) "false:"
                            `endl` showBlockWithIndent fl (n+2)
    (WhileInstr cond loop) -> "While " ++ paren (show cond)
                       `endl` showBlockWithIndent loop (n+1)
    (ForInstr var iter loop) -> "For " ++ show var ++ " in " ++ paren (show iter)
                         `endl` showBlockWithIndent loop (n+1)
    (FnInstr (FnDecl name params ret body)) -> "Function " ++ show name
                                                `endl` indent 1 ("args: " ++ paren (intercalate ", " (map show params)))
                                                `endl` indent 1 ("returns: " ++ show ret)
                                                `endl` indent 1 "body:"
                                                `endl` showBlockWithIndent body 1

showExpr :: Expr s -> String
showExpr (VarExpr n) = "Var " ++ show n
showExpr (BinOpExpr op x y) = "BinOp " ++ show op ++ " " ++ paren (show x) ++ " " ++ paren (show y)
showExpr (UnOpExpr op x) = "UnOp " ++ show op ++ " " ++ paren (show x)
showExpr (CallExpr f args) = "Call " ++ paren (show f) ++ " " ++ brack (intercalate ", " (map show args))
showExpr (ListExpr xs) = "List " ++ brack (intercalate ", " (map show xs))
showExpr (TupleExpr xs) = "Tuple " ++ paren (intercalate ", " (map show xs))
showExpr (LitExpr l) = "Lit " ++ paren (show l)

showTypeExpr :: TypeExpr s -> String
showTypeExpr (VarTExpr n) = "Var " ++ show n
showTypeExpr (ListTExpr t) = "List " ++ brack (show t)
showTypeExpr (TupleTExpr ts) = "Tuple " ++ paren (intercalate ", " (map show ts))
showTypeExpr (FnTExpr ps r) = "Fn " ++ paren (intercalate ", " (map show ps)) ++ " " ++ show r
showTypeExpr (LitTExpr l) = "Lit " ++ show l


infixl 2 `endl`

endl :: String -> String -> String
s1 `endl` s2 = s1 ++ "\n" ++ s2
