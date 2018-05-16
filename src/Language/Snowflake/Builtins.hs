{-# LANGUAGE LambdaCase #-}

module Language.Snowflake.Builtins (defaultEnv, defaultTypeEnv, defaultBindings) where

import Language.Snowflake.Typing
import Language.Snowflake.VM

import qualified Data.Map as Map

import Text.Read (readMaybe)

import Control.Monad (forM_)
import Control.Monad.Trans (lift)

import System.IO

defaultEnv = Map.fromList
    [ ("print",     printSF)
    , ("input",     inputSF)
    , ("appendInt", appendSF)
    , ("parseInt",  parseIntSF) ]

defaultTypeEnv = Map.fromList
    [ ("int", IntT)
    , ("float", FloatT)
    , ("bool", BoolT)
    , ("str", StrT) ]

defaultBindings = Map.fromList
    [ ("print",     FuncT [AnyT] NoneT)
    , ("input",     FuncT [StrT] StrT)
    , ("appendInt", FuncT [ListT IntT, IntT] (ListT IntT))
    , ("parseInt",  FuncT [StrT] IntT) ]

printSF :: Value
printSF = BuiltinVal $ \ xs -> lift . lift $ do
    forM_ xs $ \case
        StrVal s -> putStrLn s
        v        -> print v
    hFlush stdout
    return NoneVal

inputSF :: Value
inputSF = BuiltinVal $ \case
    [StrVal prompt] -> lift . lift $ do
          putStr prompt
          hFlush stdout
          input <- StrVal <$> getLine
          return input
    args -> raise TypeError ("input: expected prompt of type string, got" ++ show args)

appendSF :: Value
appendSF = BuiltinVal $ \case
    [ListVal xs, x] -> return $ ListVal (xs ++ [x])
    args -> raise TypeError ("append: expected list and value, got" ++ show args)

parseIntSF :: Value
parseIntSF = BuiltinVal $ \case
    [StrVal s] -> case readMaybe s of
        Just i  -> return (IntVal i)
        Nothing -> raise ValueError ("parseInt: unable to parse " ++ show s ++ " as an integer")
    args -> raise TypeError ("parseInt: expected string, got" ++ show args)
