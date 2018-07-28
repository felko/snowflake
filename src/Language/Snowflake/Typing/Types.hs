{-# LANGUAGE
    LambdaCase
  , TemplateHaskell
  , MultiParamTypeClasses
  , FunctionalDependencies
  #-}

module Language.Snowflake.Typing.Types
  ( Type(..)
  , showType
  , Bindings
  , TypeEnv
  , TypeCheckState(..), tcBindings, tcTypeEnv, tcExpected
  , TypeCheckErrorType(..)
  , TypeCheckError(..)
  , TypeCheck
  , eval, checkEval
  , TypeCheckable(..)
  , sandboxCheck
  , intersect
  , raiseTC
  , printError, printErrors
  ) where

import Language.Snowflake.Parser.AST

import qualified Data.ChainMap as Env

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Function (on)
import Data.Semigroup ((<>))
import Data.List (intercalate, groupBy)
import qualified Data.Map as Map

import Text.Parsec.Pos
import Debug.Trace
import System.Console.ANSI

data Type
    = IntT
    | FloatT
    | BoolT
    | StrT
    | FuncT [Type] Type
    | ListT Type
    | TupleT [Type]
    | StructT (Map.Map Name Type)
    | NoneT
    | AnyT
    deriving (Eq, Show)

showType :: Type -> String
showType IntT = "int"
showType FloatT = "float"
showType BoolT = "bool"
showType StrT = "str"
showType (FuncT ps r) = "fn(" ++ intercalate ", " (map showType ps) ++ ") -> " ++ showType r
showType (ListT t) = "[" ++ showType t ++ "]"
showType (TupleT ts) = "(" ++ intercalate ", " (map showType ts) ++ ")"
showType (StructT fields) = "{" ++ intercalate ", " (map showField (Map.assocs fields)) ++ "}"
    where showField (n, t) = show t ++ " " ++ n
showType NoneT = "None"
showType AnyT = "*"

type Bindings = Env.ChainMap Name Type
type TypeEnv  = Env.ChainMap Name Type

data TypeCheckState = TypeCheckState
    { _tcBindings :: Bindings
    , _tcTypeEnv  :: TypeEnv
    , _tcExpected :: Type }
    deriving Show
makeLenses ''TypeCheckState

data TypeCheckErrorType
    = TCScopeError
    | TCAttrError
    | TCMismatchError
    | TCUndefinedTypeError

instance Show TypeCheckErrorType where
    show TCScopeError = "Scope error"
    show TCAttrError = "Attribute error"
    show TCMismatchError = "Mismatch error"
    show TCUndefinedTypeError = "Undefined type error"

data TypeCheckError = TypeCheckError TypeCheckErrorType String Loc
    deriving Show

type TypeCheckM a = StateT TypeCheckState (ExceptT [TypeCheckError] (Reader ModuleInfo)) a
type TypeCheck n = n Loc -> TypeCheckM (n (Loc, Type))

class IsNode n => TypeCheckable n where
    check :: TypeCheck n

intersect :: Type -> Type -> TypeCheckM Type
intersect AnyT t = return t
intersect t AnyT = return t
intersect t t'
    | t == t' = return t
    | otherwise = throwError [] -- raiseTC TCMismatchError ("Failed to intersect " ++ showType t ++ " and " ++ showType t') (sp <> sp')

sandboxCheck :: TypeCheckable n => n Loc -> TypeCheckState -> TypeCheckM (n (Loc, Type))
sandboxCheck n tcs = lift $ evalStateT (check n) tcs

eval :: TypeCheckable n => n (Loc, Type) -> Type
eval = snd . nodeData

checkEval :: TypeCheckable n => n Loc -> TypeCheckM (n (Loc, Type), Type)
checkEval n = do
    cn <- check n
    return (cn, eval cn)

raiseTC :: TypeCheckErrorType -> String -> Loc -> TypeCheckM a
raiseTC t s loc = throwError [TypeCheckError t s loc]

putChunk :: Chunk -> IO ()
putChunk (ContextChunk s) = setSGR [SetUnderlining NoUnderline] >> putStr s
putChunk (ErrorChunk s) = setSGR [SetUnderlining SingleUnderline] >> putStr s

data Chunk = ContextChunk String | ErrorChunk String deriving Show
type ErrorOutput = [Chunk]

inLoc :: (Line, Column) -> Loc -> Bool
_ `inLoc` VoidLoc = False
(i, j) `inLoc` (Loc start stop)
    | sourceLine start == sourceLine stop = (i == sourceLine start) && (j >= sourceColumn start) && (j <= sourceColumn stop)
    | (i > sourceLine start) && (i < sourceLine stop) = True
    | i == sourceLine start = j >= sourceColumn start
    | i == sourceLine stop  = j <= sourceColumn stop
    | otherwise = False

-- splitSource :: String -> Line -> Loc -> ErrorOutput
-- splitSource src offset (Loc f l)
--     | sourceLine f == sourceLine l = [ beforeOffset
--                                      , [ErrorChunk $ map ((srcLines !! (sourceLine f - 1)) !!) [sourceColumn f-1..sourceColumn l-1]]
--                                      , afterOffset ]
--     | otherwise = let (before, startLine) = splitAt (sourceColumn f) (srcLines !! sourceLine f)
--                       bodyLines = map (return . ErrorChunk . (srcLines !!)) [sourceLine f+1..sourceLine l-1]
--                       (stopLine, after) = splitAt (sourceColumn l) (srcLines !! sourceLine l)
--                   in [ beforeOffset
--                      , [ContextChunk before, ErrorChunk startLine]
--                      ] ++ bodyLines ++
--                      [ [ErrorChunk stopLine, ContextChunk after]
--                      ,  afterOffset ]
--     where lineCount = length (lines src)
--           srcLines = lines src
--           startLineIdx = if sourceLine f - offset > 0 then sourceLine f - offset else 1
--           stopLineIdx  = if sourceLine l + offset <= lineCount then sourceLine l + offset else lineCount
--           beforeOffset = map (ContextChunk . (srcLines !!)) [startLineIdx..sourceLine f-1]
--           afterOffset = map (ContextChunk . (srcLines !!)) [sourceLine l+1..stopLineIdx]

sliceLoc :: String -> Loc -> String
sliceLoc src VoidLoc = ""
sliceLoc src (Loc f l)
    | f > l = ""
    | sourceLine f == sourceLine l = let line = srcLines !! (sourceLine f - 1)
                                     in drop (sourceColumn f - 1) $ take (sourceColumn l) line
    | f < l = let line = srcLines !! (sourceLine f - 1)
                  nextLoc = Loc (setSourceColumn (incSourceLine f 1) 0) l
              in drop (sourceColumn f - 1) line ++ "\n" ++ sliceLoc src nextLoc
    where srcLines = lines src

splitSource :: String -> Line -> Loc -> ErrorOutput
splitSource src offset loc@(Loc f l) = [ ContextChunk (sliceLoc src offsetToChunk)
                                       , ErrorChunk (sliceLoc src (Loc f (incSourceColumn l (-1))))
                                       , ContextChunk (sliceLoc src chunkToOffset) ]
    where lineCount = length (lines src)
          startLine = if sourceLine f - offset > 0 then sourceLine f - offset else 1
          stopLine  = if sourceLine l + offset <= lineCount then sourceLine l + offset else lineCount
          offsetToChunk = Loc (setSourceColumn (setSourceLine f startLine) 0) (incSourceColumn f (-1))
          chunkToOffset = Loc l (setSourceColumn (setSourceLine l (stopLine+1)) 0)

-- splitSource' :: String -> Line -> [Loc] -> ErrorOutput
-- splitSource' src offset locs = output
--     where lineCount = length (lines src)
--           startLine = if sourceLine f - offset > 0 then sourceLine f - offset else 1
--           stopLine  = if sourceLine l + offset <= lineCount then sourceLine l + offset else lineCount
--           idm = length (show stopLine) + 1
--           ls = drop (startLine - 1) . take (sourceLine l + offset) $ lines src
--           indexed = map (\ (i, l) -> (i, map (\ (j, c) -> (i, j, c)) $ zip [1..] l)) . zip [startLine..] $ map (++ "\n") ls
--           groups = map (fmap (groupBy groupF)) indexed
--           groupF (i, j, _) (i', j', _) = (i, j) `inLoc` loc == (i', j') `inLoc` loc
--           output = map toLine groups
--           toLine (i, l) = let lineno = show i ++ replicate (idm - length (show i)) ' ' ++ "| "
--                           in ContextChunk lineno : map toChunk l
--           toChunk [] = ContextChunk ""
--           toChunk g@((i, j, _):_)
--               | (i, j) `inLoc` loc = ErrorChunk (map (\ (_, _, c) -> c) g)
--               | otherwise          = ContextChunk (map (\ (_, _, c) -> c) g)

printErrorSource :: ModuleInfo -> Line -> Loc -> IO ()
printErrorSource (ModuleInfo src path) offset loc = mapM_ putChunk (splitSource src offset loc)

printError :: ModuleInfo -> Line -> TypeCheckError -> IO ()
printError modInfo@(ModuleInfo src path) offset (TypeCheckError typ msg loc@(Loc f l)) = do
    setSGR [SetColor Foreground Dull Red]
    let ls = sourceLine f
        lf = sourceLine l
    if ls == lf
    then putStrLn $ "File " ++ show path ++ ", line " ++ show ls ++ ":"
    else putStrLn $ "File " ++ show path ++ ", lines " ++ show ls ++ " to " ++ show lf ++ ":"
    -- setSGR [Reset]
    printErrorSource modInfo offset loc
    setSGR [SetUnderlining NoUnderline]
    putStrLn $ show typ ++ ": " ++ msg
    setSGR [Reset]

printErrors :: ModuleInfo -> Line -> [TypeCheckError] -> IO ()
printErrors modInfo offset errs = mapM_ (printError modInfo offset) errs
