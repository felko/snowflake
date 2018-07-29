{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , StandaloneDeriving
  , TypeFamilies
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
  , TypeDecl(..)
  , Param(..)
  , TypeParam(..)
  , Expr_(..)
  , Expr
  , TypeExpr_(..)
  , TypeExpr
  , TypeLiteral(..)
  , KindExpr_(..)
  , KindExpr
  , BinOp(..)
  , UnOp(..)
  , Literal(..)
  , showAST
  , terminal, terminalVoid, fromNode
  , pattern VarExpr', pattern AttrExpr', pattern BinOpExpr', pattern UnOpExpr', pattern CallExpr', pattern ListExpr', pattern TupleExpr', pattern LitExpr', pattern StructExpr'
  , pattern VarExpr, pattern AttrExpr, pattern BinOpExpr, pattern UnOpExpr, pattern CallExpr, pattern ListExpr, pattern TupleExpr, pattern LitExpr, pattern StructExpr
  , pattern VarTExpr', pattern ListTExpr', pattern TupleTExpr', pattern FnTExpr', pattern LitTExpr', pattern StructTExpr'
  , pattern VarTExpr, pattern ListTExpr, pattern TupleTExpr, pattern FnTExpr, pattern LitTExpr, pattern StructTExpr
  , pattern TypeKExpr'
  , pattern TypeKExpr
  ) where

import Data.AST

import Data.Bifunctor
import Data.Functor.Foldable
import Data.Functor.Compose
import Data.Functor.Classes

import Data.Semigroup
import Data.Int
import Data.List (intercalate)
import qualified Data.Map as Map

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

instance Bifunctor f => IsNode (AST f) where
    nodeData = extract
    nodeSet (_ :> t) s = s :> t

instance IsNode (Decl n) where
    nodeData (Decl (Node _ s)) = s
    nodeSet (Decl (Node n _)) s = Decl (Node n s)

instance IsNode Program where
    nodeData (Program d) = nodeData d
    nodeSet (Program d) s = Program (nodeSet d s)

instance IsNode Param where
    nodeData (Param t n) = nodeData t
    nodeSet (Param t n) s = Param (nodeSet t s) n

data Node a n = Node
    { _nodeValue :: n
    , _nodeData  :: a }
    deriving (Show, Eq, Functor)
--
-- newtype AST n s = AST { unAST :: Fix (Compose (Node s) n) }
--
-- type instance Base (AST n s) = n
--
-- instance Functor n => Recursive (AST n s) where
--     project (AST t) = AST <$> n
--         where Compose (Node n s) = project t
--
-- instance Functor n => Functor (AST n) where
--     fmap f (AST (Fix (Compose (Node n s)))) = AST (Fix (Compose (Node n' (f s))))
--         where n' = fmap (unAST . fmap f . AST) n

terminal :: Bifunctor n => (forall b. a -> n Loc b) -> Node Loc a -> AST n Loc
terminal constr x = fromNode (constr <$> x)

terminalVoid :: Bifunctor n => (forall b. a -> n Loc b) -> a -> AST n Loc
terminalVoid constr x = fromNode (Node (constr x) VoidLoc)

fromNode :: Bifunctor f => Node a (f a (AST f a)) -> AST f a
fromNode (Node n s) = s :> n --AST . Fix . Compose $ Node (fmap unAST n) s

data ModuleInfo = ModuleInfo
    { _modSource :: String
    , _modPath   :: FilePath }
    deriving Show

newtype Program a = Program (Decl Block a)

deriving instance Functor Program

newtype Block a = Block [Decl Instruction a]

deriving instance Functor Block

newtype Decl f a = Decl { unDecl :: Node a (f a) }

instance Functor f => Functor (Decl f) where
    fmap f (Decl (Node n s)) = Decl (Node (fmap f n) (f s))

data Instruction a
    = DeclareInstr (TypeExpr a) Name (Expr a)
    | AssignInstr Name (Expr a)
    | ReturnInstr (Expr a)
    | ExprInstr (Expr a)
    | CondInstr (Expr a) (Decl Block a) (Decl Block a)
    | WhileInstr (Expr a) (Decl Block a)
    | ForInstr Name (Expr a) (Decl Block a)
    | FnInstr (FnDecl a)
    | TypeInstr (TypeDecl a)

instance Functor Instruction where
    fmap f (DeclareInstr t n v) = DeclareInstr (fmap f t) n (fmap f v)
    fmap f (AssignInstr n v) = AssignInstr n (fmap f v)
    fmap f (ReturnInstr v) = ReturnInstr (fmap f v)
    fmap f (ExprInstr e) = ExprInstr (fmap f e)
    fmap f (CondInstr c tr fl) = CondInstr (fmap f c) (fmap f tr) (fmap f fl)
    fmap f (WhileInstr c l) = WhileInstr (fmap f c) (fmap f l)
    fmap f (ForInstr n i l) = ForInstr n (fmap f i) (fmap f l)
    fmap f (FnInstr d) = FnInstr (fmap f d)
    fmap f (TypeInstr d) = TypeInstr (fmap f d)

data FnDecl a = FnDecl Name [TypeParam Loc] [Param a] (TypeExpr a) (Decl Block a)

instance Functor FnDecl where
    fmap f (FnDecl n gs ps r b) = FnDecl n gs (fmap (fmap f) ps) (fmap f r) (fmap f b)

data TypeDecl a = TypeDecl Name (Map.Map Name (TypeExpr a))

instance Functor TypeDecl where
    fmap f (TypeDecl n fs) = TypeDecl n (fmap (fmap f) fs)

data Param a = Param
    { _paramType :: TypeExpr a
    , _paramName :: Name }

instance Functor Param where
    fmap f (Param t n) = Param (fmap f t) n

data TypeParam a = TypeParam
    { _typeParamKind :: KindExpr a
    , _typeParamName :: Name }

instance Functor TypeParam where
    fmap f (TypeParam k n) = TypeParam (fmap f k) n

instance Show (Param a) where
    show (Param t n) = "Param " ++ paren (show t) ++ " " ++ show n

instance Show (TypeParam a) where
    show (TypeParam k n) = "TypeParam " ++ paren (show k) ++ " " ++ show n

data Expr_ a expr
    = VarExpr_ Name
    | AttrExpr_ expr Name
    | BinOpExpr_ BinOp expr expr
    | UnOpExpr_ UnOp expr
    | CallExpr_ expr [TypeExpr a] [expr]
    | ListExpr_ [expr]
    | TupleExpr_ [expr]
    | LitExpr_ Literal
    | StructExpr_ (Map.Map Name expr)
    --deriving (Functor, Foldable, Traversable)

instance Bifunctor Expr_ where
    bimap f g (VarExpr_ n) = VarExpr_ n
    bimap f g (AttrExpr_ x a) = AttrExpr_ (g x) a
    bimap f g (BinOpExpr_ op x y) = BinOpExpr_ op (g x) (g y)
    bimap f g (UnOpExpr_ op x) = UnOpExpr_ op (g x)
    bimap f g (CallExpr_ fx gs xs) = CallExpr_ (g fx) (fmap (fmap f) gs) (fmap g xs)
    bimap f g (ListExpr_ xs) = ListExpr_ (fmap g xs)
    bimap f g (TupleExpr_ xs) = TupleExpr_ (fmap g xs)
    bimap f g (LitExpr_ lit) = LitExpr_ lit
    bimap f g (StructExpr_ as) = StructExpr_ (fmap g as)

type Expr a = AST Expr_ a

-- State patterns
pattern VarExpr' :: Name -> a -> Expr a
pattern VarExpr' v s <- s :> VarExpr_ v

pattern AttrExpr' :: Expr a -> Name -> a -> Expr a
pattern AttrExpr' x a s <- s :> AttrExpr_ x a

pattern BinOpExpr' :: BinOp -> Expr a -> Expr a -> a -> Expr a
pattern BinOpExpr' op x y s <- s :> BinOpExpr_ op x y

pattern UnOpExpr' :: UnOp -> Expr a -> a -> Expr a
pattern UnOpExpr' op x s <- s :> UnOpExpr_ op x

pattern CallExpr' :: Expr a -> [TypeExpr a] -> [Expr a] -> a -> Expr a
pattern CallExpr' f gs xs s <- s :> CallExpr_ f gs xs

pattern ListExpr' :: [Expr a] -> a -> Expr a
pattern ListExpr' xs s <- s :> ListExpr_ xs

pattern TupleExpr' :: [Expr a] -> a -> Expr a
pattern TupleExpr' xs s <- s :> TupleExpr_ xs

pattern LitExpr' :: Literal -> a -> Expr a
pattern LitExpr' lit s <- s :> LitExpr_ lit

pattern StructExpr' :: Map.Map Name (Expr a) -> a -> Expr a
pattern StructExpr' assocs s <- s :> StructExpr_ assocs

-- Pure patterns
pattern VarExpr :: Name -> Expr a
pattern VarExpr v <- VarExpr' v _

pattern AttrExpr :: Expr a -> Name -> Expr a
pattern AttrExpr x a <- AttrExpr' x a _

pattern BinOpExpr :: BinOp -> Expr a -> Expr a -> Expr a
pattern BinOpExpr op x y <- BinOpExpr' op x y _

pattern UnOpExpr :: UnOp -> Expr a -> Expr a
pattern UnOpExpr op x <- UnOpExpr' op x _

pattern CallExpr :: Expr a -> [TypeExpr a] -> [Expr a] -> Expr a
pattern CallExpr f gs xs <- CallExpr' f gs xs _

pattern ListExpr :: [Expr a] -> Expr a
pattern ListExpr xs <- ListExpr' xs _

pattern TupleExpr :: [Expr a] -> Expr a
pattern TupleExpr xs <- TupleExpr' xs _

pattern LitExpr :: Literal -> Expr a
pattern LitExpr lit <- LitExpr' lit _

pattern StructExpr :: Map.Map Name (Expr a) -> Expr a
pattern StructExpr assocs <- StructExpr' assocs _

instance {-# OVERLAPS #-} Show (Expr a) where
    show = showExpr

data TypeExpr_ a expr
    = VarTExpr_ Name
    | ListTExpr_ expr
    | TupleTExpr_ [expr]
    | FnTExpr_  [TypeParam Loc] [expr] expr
    | LitTExpr_ TypeLiteral
    | StructTExpr_ (Map.Map Name expr)
    --deriving (Functor, Foldable, Traversable)

instance Bifunctor TypeExpr_ where
    bimap f g (VarTExpr_ v) = VarTExpr_ v
    bimap f g (ListTExpr_ t) = ListTExpr_ (g t)
    bimap f g (TupleTExpr_ ts) = TupleTExpr_ (fmap g ts)
    bimap f g (FnTExpr_ gs ps r) = FnTExpr_ gs (fmap g ps) (g r)
    bimap f g (LitTExpr_ l) = LitTExpr_ l
    bimap f g (StructTExpr_ fs) = StructTExpr_ (fmap g fs)

type TypeExpr a = AST TypeExpr_ a

-- State patterns
pattern VarTExpr' :: Name -> a -> TypeExpr a
pattern VarTExpr' v s <- s :> VarTExpr_ v

pattern ListTExpr' :: TypeExpr a -> a -> TypeExpr a
pattern ListTExpr' x s <- s :> ListTExpr_ x

pattern TupleTExpr' :: [TypeExpr a] -> a -> TypeExpr a
pattern TupleTExpr' xs s <- s :> TupleTExpr_ xs

pattern FnTExpr' :: [TypeParam Loc] -> [TypeExpr a] -> TypeExpr a -> a -> TypeExpr a
pattern FnTExpr' tps xs ret s <- s :> FnTExpr_ tps xs ret

pattern LitTExpr' :: TypeLiteral -> a -> TypeExpr a
pattern LitTExpr' lit s <- s :> LitTExpr_ lit

pattern StructTExpr' :: Map.Map Name (TypeExpr a) -> a -> TypeExpr a
pattern StructTExpr' fields s <- s :> StructTExpr_ fields

-- Pure patterns
pattern VarTExpr :: Name -> TypeExpr a
pattern VarTExpr v <- VarTExpr' v _

pattern ListTExpr :: TypeExpr a -> TypeExpr a
pattern ListTExpr x <- ListTExpr' x _

pattern TupleTExpr :: [TypeExpr a] -> TypeExpr a
pattern TupleTExpr xs <- TupleTExpr' xs _

pattern FnTExpr :: [TypeParam Loc] -> [TypeExpr a] -> TypeExpr a -> TypeExpr a
pattern FnTExpr tps xs ret <- FnTExpr' tps xs ret _

pattern LitTExpr :: TypeLiteral -> TypeExpr a
pattern LitTExpr lit <- LitTExpr' lit _

pattern StructTExpr :: Map.Map Name (TypeExpr a) -> TypeExpr a
pattern StructTExpr fields <- StructTExpr' fields _

instance {-# OVERLAPS #-} Show (TypeExpr a) where
    show = showTypeExpr

data KindExpr_ a expr
    = TypeKExpr_

instance Bifunctor KindExpr_ where
    bimap f g TypeKExpr_ = TypeKExpr_

type KindExpr a = AST KindExpr_ a

pattern TypeKExpr' :: a -> KindExpr a
pattern TypeKExpr' s <- s :> TypeKExpr_

pattern TypeKExpr :: KindExpr a
pattern TypeKExpr <- TypeKExpr' _

instance {-# OVERLAPS #-} Show (KindExpr a) where
    show = showKindExpr

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

angle :: String -> String
angle = surround "<" ">"

showAST :: Program a -> String
showAST (Program instrs) = showBlock instrs

showBlock :: Decl Block a -> String
showBlock block = showBlockWithIndent block 0

showBlockWithIndent :: Decl Block a -> Int -> String
showBlockWithIndent (Decl (Node (Block block) _)) n = intercalate "\n" $ map (indent n . flip showInstrWithIndent n . _nodeValue . unDecl) block

showInstrWithIndent :: Instruction a -> Int -> String
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
    (FnInstr (FnDecl name tparams params ret body)) -> "Function " ++ show name
                                                `endl` indent 1 ("type params: " ++ show tparams)
                                                `endl` indent 1 ("params:      " ++ paren (intercalate ", " (map show params)))
                                                `endl` indent 1 ("returns:     " ++ show ret)
                                                `endl` indent 1 "body:"
                                                `endl` showBlockWithIndent body 1
    (TypeInstr (TypeDecl name fields)) -> "Type " ++ show name
                                            `endl` indent 1 ("fields: " ++ paren (show fields))

showExpr :: Expr a -> String
showExpr (VarExpr n) = "Var " ++ show n
showExpr (AttrExpr x a) = "Attr " ++ show x ++ " " ++ show a
showExpr (BinOpExpr op x y) = "BinOp " ++ show op ++ " " ++ paren (show x) ++ " " ++ paren (show y)
showExpr (UnOpExpr op x) = "UnOp " ++ show op ++ " " ++ paren (show x)
showExpr (CallExpr f [] args) = "Call " ++ paren (show f) ++ " " ++ brack (intercalate ", " (map show args))
showExpr (CallExpr f gs args) = "Call " ++ paren (show f) ++ angle (intercalate ", " (map showTypeExpr gs)) ++ " " ++ brack (intercalate ", " (map show args))
showExpr (ListExpr xs) = "List " ++ brack (intercalate ", " (map show xs))
showExpr (TupleExpr xs) = "Tuple " ++ paren (intercalate ", " (map show xs))
showExpr (LitExpr l) = "Lit " ++ paren (show l)
showExpr (StructExpr assocs) = "Struct " ++ paren (show assocs)

showTypeExpr :: TypeExpr a -> String
showTypeExpr (VarTExpr v) = "Var " ++ show v
showTypeExpr (ListTExpr t) = "List " ++ brack (show t)
showTypeExpr (TupleTExpr ts) = "Tuple " ++ paren (intercalate ", " (map show ts))
showTypeExpr (FnTExpr [] ps r) = "Fn " ++ paren (intercalate ", " (map show ps)) ++ " " ++ show r
showTypeExpr (FnTExpr tps ps r) = "Fn " ++ angle (intercalate ", " (map showTypeParam tps)) ++ paren (intercalate ", " (map show ps)) ++ " " ++ show r
    where showTypeParam (TypeParam k n) = show k ++ " " ++ n
showTypeExpr (LitTExpr l) = "Lit " ++ show l
showTypeExpr (StructTExpr fields) = "Struct " ++ paren (show fields)

showKindExpr :: KindExpr a -> String
showKindExpr TypeKExpr = "Type"

infixl 2 `endl`

endl :: String -> String -> String
s1 `endl` s2 = s1 ++ "\n" ++ s2
