{-# LANGUAGE
    FlexibleContexts
  #-}

-- Inspired from https://github.com/romac/lfc-haskell

module Data.AST
  ( AST
  , module Control.Comonad
  , module Control.Comonad.Cofree
  , annotate, annotateM, annotateM_
  ) where

import Data.Functor.Foldable

import Control.Comonad
import Control.Comonad.Cofree

type AST f a = Cofree f a

annotate :: Recursive t => (t -> a) -> t -> AST (Base t) a
annotate alg t = alg t :< fmap (annotate alg) (project t)

annotateM :: (Recursive t, Monad m, Traversable (Base t)) => (Base t (m a) -> m a) -> t -> m (AST (Base t) a)
annotateM f x = sequence (annotate (cata f) x)

annotateM_ :: (Recursive t, Traversable (Base t), Monad m) => (Cofree (Base t) () -> m a) -> t -> m (Cofree (Base t) a)
annotateM_ f x = sequence (extend f (annotate (const ()) x))
