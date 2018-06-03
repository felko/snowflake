module Language.Snowflake.Parser.Rules
  ( loc
  , program
  , block
  , fnDecl
  , instruction, assignInstr, returnInstr, exprInstr, condInstr, whileInstr, forInstr, fnInstr
  , expr, term, varExpr, callExpr, listExpr, tupleExpr, litExpr
  , typeExpr, varTExpr, listTExpr, tupleTExpr, fnTExpr, litTExpr
  , typeLiteral
  , literal, intLit, boolLit ) where

import Language.Snowflake.Parser.AST
import Language.Snowflake.Parser.Lexer

import Control.Applicative ((<$>), (<*>))
import Data.Functor.Foldable
import Data.Functor.Compose
import Data.Functor.Identity (Identity)

import Text.Parsec as P
import Text.Parsec.Expr
import Text.Parsec.String

import Data.Semigroup ((<>))

loc :: Parser a -> Parser (Node Loc a)
loc p = do
    start <- getPosition
    x <- p
    stop <- getPosition
    return $ Node x (Loc start stop)

decl :: Parser (n Loc) -> Parser (Decl n Loc)
decl p = Decl <$> loc p

ast :: Functor n => Parser (Node s (n (AST n s))) -> Parser (AST n s)
ast = fmap fromNode

program :: Parser (Program Loc)
program = Program <$> decl (Block <$> many instruction)

block :: Parser (Decl Block Loc)
block = decl $ (Block <$> (try (fmap return instruction)
                      <|> braces (many instruction)
                      <?> "block"))

fnDecl :: Parser (FnDecl Loc)
fnDecl = do
    reserved "fn"
    fnName <- identifier
    fnParams <- parens (commaSep param)
    pos <- getPosition
    fnRetType <- try (reservedOp "->" >> typeExpr) <|> pure (terminalVoid LitTExpr_ NoneTLit)
    fnBlock <- block
    return (FnDecl fnName fnParams fnRetType fnBlock)

param :: Parser (Param Loc)
param = Param <$> lexeme typeExpr <*> identifier

instruction, declareInstr, assignInstr, returnInstr, exprInstr, condInstr, whileInstr, forInstr, fnInstr :: Parser (Decl Instruction Loc)
instruction =  try exprInstr
           <|> try declareInstr
           <|> try assignInstr
           <|> try returnInstr
           <|> try condInstr
           <|> try whileInstr
           <|> try fnInstr
           <|> forInstr
           <?> "instruction"

declareInstr = decl $ do
    typ  <- typeExpr
    name <- identifier
    reservedOp "="
    value <- lexeme expr
    semi
    return $ DeclareInstr typ name value

assignInstr = decl $ do
    name <- identifier
    reservedOp "="
    value <- lexeme expr
    semi
    return $ AssignInstr name value

returnInstr = decl $ reserved "return" >> ReturnInstr <$> (expr <* semi)

exprInstr = decl $ ExprInstr <$> (expr <* semi)

condInstr = decl $ do
    reserved "if"
    cond <- parens expr
    tr <- block
    fl <- try (reserved "else" >> block) <|> pure (Decl (Node (Block []) VoidLoc))
    return $ CondInstr cond tr fl

whileInstr = decl $ reserved "while" >> WhileInstr <$> parens expr <*> block

forInstr = decl $ do
    reserved "for"
    var <- identifier
    reserved "in"
    iter <- lexeme expr
    loop <- block
    return $ ForInstr var iter loop

fnInstr = decl $ FnInstr <$> fnDecl

unary :: String -> UnOp -> Operator String () Identity (Expr Loc)
binary s op assoc = flip Infix assoc $ do
  reservedOp s
  return $ \ x y -> fromNode $ Node (BinOpExpr_ op x y) (astNodeData x <> astNodeData y)
--unary  s op       = Prefix (loc (reservedOp s) >>= \ (Loc _ sp) -> return (\ x -> Loc (UnOpExpr op (Loc x sp)) sp)) -- (loc (reservedOp s) >>= return . Loc (UnOpExpr  op) . _locSpan)
unary s op = Prefix $ do
    Node _ l <- loc (reservedOp s)
    return $ \ x -> fromNode $ Node (UnOpExpr_ op x) (l <> astNodeData x)

opTable = [ [ binary "^"   PowOp AssocRight ]
          , [ binary "*"   MulOp AssocLeft
            , binary "/"   DivOp AssocLeft ]
          , [ binary "+"   AddOp AssocLeft
            , binary "-"   SubOp AssocLeft ]
          , [ unary  "+"   PosOp
            , unary  "-"   NegOp ]
          , [ binary "<"   LTOp AssocNone
            , binary "<="  LEOp AssocNone
            , binary ">"   GTOp AssocNone
            , binary ">="  GEOp AssocNone
            , binary "=="  EQOp AssocNone
            , binary "!="  NEQOp AssocNone ]
          , [ binary "and" AndOp AssocRight ]
          , [ binary "or"  OrOp AssocRight ]
          , [ unary  "not" NotOp ]
          ]

expr :: Parser (Expr Loc)
term, varExpr, callExpr, listExpr, tupleExpr, litExpr :: Parser (Expr Loc)
expr = buildExpressionParser opTable term

term =  try callExpr
    <|> try varExpr
    <|> try listExpr
    <|> litExpr
    <|> try (parens expr)
    <|> try tupleExpr
    <?> "expression"

varExpr = terminal VarExpr_ <$> loc identifier
callExpr = do
    Node fn l <- loc identifier
    Node args l' <- loc $ parens (commaSep expr)
    return . fromNode $ Node (CallExpr_ (terminal VarExpr_ (Node fn l)) args) (l <> l')
listExpr = fromNode . fmap ListExpr_ <$> loc (brackets (commaSep expr))
tupleExpr = fromNode . fmap TupleExpr_ <$> loc (parens (commaSep expr))
litExpr = terminal LitExpr_ <$> loc literal

literal, intLit, floatLit, boolLit, strLit, noneLit :: Parser Literal
literal =  try floatLit
       <|> try intLit
       <|> try strLit
       <|> boolLit
       <|> noneLit
       <?> "literal"

intLit = IntLit . fromInteger <$> integer
floatLit = FloatLit . realToFrac <$> float
boolLit = BoolLit <$> ((reserved "true" >> return True) <|> (reserved "false" >> return False))
strLit = StrLit <$> stringLiteral
noneLit = reserved "none" >> return NoneLit

typeExpr, varTExpr, listTExpr, tupleTExpr, fnTExpr, litTExpr :: Parser (TypeExpr Loc)
typeExpr =  try fnTExpr
        <|> try litTExpr
        <|> varTExpr
        <|> listTExpr
        <|> try tupleTExpr
        <|> parens typeExpr
        <?> "type expression"

varTExpr = terminal VarTExpr_ <$> loc identifier
listTExpr = fromNode . fmap ListTExpr_ <$> loc (brackets typeExpr)
tupleTExpr = fromNode . fmap TupleTExpr_ <$> loc (parens (commaSep typeExpr))
fnTExpr = do
    Node _ l <- loc $ reserved "fn"
    paramTypes <- parens (many typeExpr)
    reservedOp "->"
    retType <- typeExpr
    return . fromNode $ Node (FnTExpr_ paramTypes retType) (l <> astNodeData retType)
litTExpr = terminal LitTExpr_ <$> loc typeLiteral

typeLiteral :: Parser TypeLiteral
typeLiteral =  try (reserved "int" >> return IntTLit)
           <|> try (reserved "float" >> return FloatTLit)
           <|> try (reserved "bool" >> return BoolTLit)
           <|> try (reserved "str" >> return StrTLit)
           <|> try (reserved "none" >> return NoneTLit)
           <?> "type literal"
