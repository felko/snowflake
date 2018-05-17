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
import Data.Functor.Identity (Identity)

import Text.Parsec as P
import Text.Parsec.Expr
import Text.Parsec.String

loc :: Parser a -> Parser (Loc a)
loc p = do
    start <- getPosition
    x <- p
    stop <- getPosition
    return $ Loc x [(start, stop)]

program :: Parser Program
program = Program <$> loc (many (loc instruction))

block :: Parser Block
block =  try (fmap return (loc instruction))
     <|> braces (many (loc instruction))
     <?> "block"

fnDecl :: Parser FnDecl
fnDecl = do
    reserved "fn"
    fnName <- identifier
    fnParams <- parens (commaSep (loc param))
    pos <- getPosition
    fnRetType <- try (reservedOp "->" >> loc typeExpr) <|> pure (Loc (LitTExpr (Loc NoneTLit [(pos, pos)])) [(pos, pos)])
    fnBlock <- loc block
    return (FnDecl fnName fnParams fnRetType fnBlock)

param :: Parser Param
param = Param <$> lexeme (loc typeExpr) <*> identifier

instruction, declareInstr, assignInstr, returnInstr, exprInstr, condInstr, whileInstr, forInstr, fnInstr :: Parser Instruction
instruction =  try exprInstr
           <|> try declareInstr
           <|> try assignInstr
           <|> try returnInstr
           <|> try condInstr
           <|> try whileInstr
           <|> try fnInstr
           <|> forInstr
           <?> "instruction"

declareInstr = do
    typ  <- loc typeExpr
    name <- identifier
    reservedOp "="
    value <- lexeme expr
    semi
    return $ DeclareInstr typ name value

assignInstr = do
    name <- identifier
    reservedOp "="
    value <- lexeme expr
    semi
    return $ AssignInstr name value

returnInstr = reserved "return" >> ReturnInstr <$> (expr <* semi)

exprInstr = ExprInstr <$> (expr <* semi)

condInstr = do
    reserved "if"
    cond <- parens expr
    tr <- loc block
    fl <- loc $ try (reserved "else" >> block) <|> pure []
    return $ CondInstr cond tr fl

whileInstr = reserved "while" >> WhileInstr <$> parens expr <*> loc block

forInstr = do
    reserved "for"
    var <- identifier
    reserved "in"
    iter <- lexeme expr
    loop <- loc block
    return $ ForInstr var iter loop

fnInstr = FnInstr <$> loc fnDecl

unary :: String -> UnOp -> Operator String () Identity (Loc Expr)
binary s op assoc = flip Infix assoc $ do
  Loc _ sp <- loc (reservedOp s)
  return $ \ x y -> Loc (BinOpExpr op x y) sp
--unary  s op       = Prefix (loc (reservedOp s) >>= \ (Loc _ sp) -> return (\ x -> Loc (UnOpExpr op (Loc x sp)) sp)) -- (loc (reservedOp s) >>= return . Loc (UnOpExpr  op) . _locSpan)
unary s op = Prefix $ do
    Loc _ sp <- loc (reservedOp s)
    return $ \ x -> Loc (UnOpExpr op x) sp

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

expr :: Parser (Loc Expr)
term, varExpr, callExpr, listExpr, tupleExpr, litExpr :: Parser Expr
expr = buildExpressionParser opTable (loc term)

term =  try callExpr
    <|> try varExpr
    <|> try listExpr
    <|> litExpr
    <|> try (parens (_locNode <$> expr))
    <|> try tupleExpr
    <?> "expression"

varExpr = VarExpr <$> identifier
callExpr = do
    Loc fn sp <- loc identifier
    args <- parens (commaSep expr)
    return $ CallExpr (Loc (VarExpr fn) sp) args
listExpr = ListExpr <$> brackets (commaSep expr)
tupleExpr = TupleExpr <$> parens (commaSep expr)
litExpr = LitExpr <$> loc literal

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

typeExpr, varTExpr, listTExpr, tupleTExpr, fnTExpr, litTExpr :: Parser TypeExpr
typeExpr =  try fnTExpr
        <|> try litTExpr
        <|> varTExpr
        <|> listTExpr
        <|> try tupleTExpr
        <|> parens typeExpr
        <?> "type expression"

varTExpr = VarTExpr <$> identifier
listTExpr = ListTExpr <$> brackets (loc typeExpr)
tupleTExpr = TupleTExpr <$> parens (commaSep (loc typeExpr))
fnTExpr = do
    reserved "fn"
    paramTypes <- parens (many (loc typeExpr))
    reservedOp "->"
    retType <- loc typeExpr
    return $ FnTExpr paramTypes retType
litTExpr = LitTExpr <$> loc typeLiteral

typeLiteral :: Parser TypeLiteral
typeLiteral =  try (reserved "int" >> return IntTLit)
           <|> try (reserved "float" >> return FloatTLit)
           <|> try (reserved "bool" >> return BoolTLit)
           <|> try (reserved "str" >> return StrTLit)
           <|> try (reserved "none" >> return NoneTLit)
           <?> "type literal"
