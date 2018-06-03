{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , TemplateHaskell
  #-}

module Language.Snowflake.REPL.Types
    ( REPL
    , REPLState(..), replFile, replSettings, replBindings, replLine, replErrorOffset, replRunning
    , REPLInput(..)
    , REPLCommand(..)
    ) where

import Language.Snowflake.Parser.AST
import Language.Snowflake.Typing.Types
import Language.Snowflake.VM

import Control.Lens
import Control.Monad.State
import Control.Monad.Except

import System.Console.Haskeline

data REPLState = REPLState
    { _replFile        :: Maybe FilePath
    , _replSettings    :: Settings (StateT REPLState (StateT VMState (ExceptT VMException IO)))
    , _replBindings    :: Bindings
    , _replLine        :: Int
    , _replErrorOffset :: Int
    , _replRunning     :: Bool }
makeLenses ''REPLState

type REPL a = InputT (StateT REPLState (StateT VMState (ExceptT VMException IO))) a

data REPLInput
    = Instr   (Decl Instruction Loc, String)
    | Expr    (Expr Loc, String)
    | Command REPLCommand
    | NoInput

data REPLCommand
    = Type (Expr Loc, String)
    | Load FilePath
    | Reload
    | Quit
    deriving Show

instance MonadException m => MonadException (ExceptT e m) where
    controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap ExceptT . run . runExceptT)
                    in fmap runExceptT $ f run'

instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in fmap (flip runStateT s) $ f run'

instance MonadError VMException (InputT (StateT REPLState (StateT VMState (ExceptT VMException IO)))) where
    throwError = lift . lift . throwError
    m `catchError` h = do
        settings <- lift $ gets _replSettings
        let s = runInputT settings m
        lift $ s `catchError` (runInputT settings . h)
