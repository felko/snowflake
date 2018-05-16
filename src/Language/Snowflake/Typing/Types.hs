{-# LANGUAGE TemplateHaskell #-}

module Language.Snowflake.Typing.Types
  ( Type(..)
  , showType
  , Bindings
  , TypeEnv
  , TypeCheckState(..), tcBindings, tcTypeEnv, tcExpected
  , TypeCheckErrorType(..)
  , TypeCheckError(..)
  , TypeCheck
  , intersect
  , raiseTC
  , printError, printErrors
  ) where

-- todo: replace `TypeCheckError TypeCheckErrorType String Span` by `Loc TypeCheckError`

import Language.Snowflake.Parser.AST

import qualified Data.ChainMap as Env

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Function (on)
import Data.Semigroup ((<>))
import Data.List (intercalate, groupBy)

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
    | TCMismatchError
    | TCUndefinedTypeError

instance Show TypeCheckErrorType where
    show TCScopeError = "Scope error"
    show TCMismatchError = "Mismatch error"
    show TCUndefinedTypeError = "Undefined type error"

data TypeCheckError = TypeCheckError TypeCheckErrorType String Span
    deriving Show

type TypeCheck a = StateT TypeCheckState (ExceptT [TypeCheckError] (Reader ModuleInfo)) a

intersect :: Loc Type -> Loc Type -> TypeCheck (Loc Type)
intersect (Loc AnyT sp) (Loc t sp') = return (Loc t (sp <> sp'))
intersect (Loc t sp) (Loc AnyT sp') = return (Loc t (sp <> sp'))
intersect (Loc t sp) (Loc t' sp')
    | t == t'   = return (Loc t (sp <> sp'))
    | otherwise = throwError [] -- raiseTC TCMismatchError ("Failed to intersect " ++ showType t ++ " and " ++ showType t') (sp <> sp')

raiseTC :: TypeCheckErrorType -> String -> Span -> TypeCheck a
raiseTC t s sp = throwError [TypeCheckError t s sp]

putChunk :: Chunk -> IO ()
putChunk (ContextChunk s) = setSGR [SetUnderlining NoUnderline] >> putStr s
putChunk (ErrorChunk s) = setSGR [SetUnderlining SingleUnderline] >> putStr s

firstLast :: Span -> Range
firstLast sp = (fst $ head sp, snd $ last sp)

data Chunk = ContextChunk String | ErrorChunk String deriving Show
type ChunkLine   = [Chunk]
type ErrorOutput = [ChunkLine]

inRange :: (Line, Column) -> Range -> Bool
(i, j) `inRange` (start, stop)
    | sourceLine start == sourceLine stop = (i == sourceLine start) && (j >= sourceColumn start) && (j <= sourceColumn stop)
    | (i > sourceLine start) && (i < sourceLine stop) = True
    | i == sourceLine start = j >= sourceColumn start
    | i == sourceLine stop  = j <= sourceColumn stop
    | otherwise = False

inSpan :: (Line, Column) -> Span -> Bool
pos `inSpan` sp = any (pos `inRange`) sp

splitSource :: String -> Line -> Span -> ErrorOutput
splitSource src offset sp = output
    where (f, l) = firstLast sp
          lineCount = length (lines src)
          startLine = if sourceLine f - offset > 0 then sourceLine f - offset else 1
          stopLine  = if sourceLine l + offset <= lineCount then sourceLine l + offset else lineCount
          idm = length (show stopLine) + 1
          ls = drop (startLine - 1) . take (sourceLine l + offset) $ lines src
          indexed = map (\ (i, l) -> (i, map (\ (j, c) -> (i, j, c)) $ zip [1..] l)) . zip [startLine..] $ map (++ "\n") ls
          groups = map (fmap (groupBy groupF)) indexed
          groupF (i, j, _) (i', j', _) = (i, j) `inSpan` sp == (i', j') `inSpan` sp
          output = map toLine groups
          toLine (i, l) = let lineno = show i ++ replicate (idm - length (show i)) ' ' ++ "| "
                          in ContextChunk lineno : map toChunk l
          toChunk [] = ContextChunk ""
          toChunk g@((i, j, _):_)
              | (i, j) `inSpan` sp = ErrorChunk (map (\ (_, _, c) -> c) g)
              | otherwise          = ContextChunk (map (\ (_, _, c) -> c) g)

printErrorSource :: ModuleInfo -> Line -> Span -> IO ()
printErrorSource (ModuleInfo src path) offset sp = mapM_ putChunk (concat $ splitSource src offset sp)

printError :: ModuleInfo -> Line -> TypeCheckError -> IO ()
printError modInfo@(ModuleInfo src path) offset (TypeCheckError typ msg sp) = do
    setSGR [SetColor Foreground Dull Red]
    let (spFst, spLst) = firstLast sp
        ls = sourceLine spFst
        lf = sourceLine spLst
    if ls == lf
    then putStrLn $ "File " ++ show path ++ ", line " ++ show ls ++ ":"
    else putStrLn $ "File " ++ show path ++ ", lines " ++ show ls ++ " to " ++ show lf ++ ":"
    -- setSGR [Reset]
    printErrorSource modInfo offset sp
    setSGR [SetUnderlining NoUnderline]
    putStrLn $ show typ ++ ": " ++ msg
    setSGR [Reset]

printErrors :: ModuleInfo -> Line -> [TypeCheckError] -> IO ()
printErrors modInfo offset errs = mapM_ (printError modInfo offset) errs
