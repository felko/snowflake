{-# LANGUAGE
    FlexibleContexts
  , PatternSynonyms
  , ViewPatterns
  #-}

-- Inspired from https://github.com/romac/lfc-haskell

module Data.AST
  ( AST
  , module Control.Comonad
  , module Control.Comonad.Cofree
  --, annotate, annotateM, annotateM_
  , pattern (:>)
  ) where

import Data.Bifunctor
import Data.Functor.Foldable

import Control.Comonad
import Control.Comonad.Cofree

newtype AST f a = AST { unAST :: Cofree (f a) a }

instance Bifunctor f => Functor (AST f) where
    fmap f (s :> n) = f s :> bimap f (fmap f) n

instance Bifunctor f => Comonad (AST f) where
    extract (s :> _) = s
    duplicate n@(_ :> c) = n :> first (const n) (second duplicate c)
        --where n = _ --extend (f . AST) _

pattern (:>) :: Bifunctor f => a -> f a (AST f a) -> AST f a
pattern s :> n <- AST (s :< (second AST -> n)) where
    s :> n = AST (s :< (second unAST n))

-- cofree f a = a :< f (cofree f a)
-- cofree (f a) a = a :< f a (cofree (f a) a)

-- annotate :: Recursive t => (t -> a) -> t -> AST (Base t) a
-- annotate alg t = alg t :< fmap (annotate alg) (project t)
--
-- annotateM :: (Recursive t, Monad m, Traversable (Base t)) => (Base t (m a) -> m a) -> t -> m (AST (Base t) a)
-- annotateM f x = sequence (annotate (cata f) x)
--
-- annotateM_ :: (Recursive t, Traversable (Base t), Monad m) => (Cofree (Base t) () -> m a) -> t -> m (Cofree (Base t) a)
-- annotateM_ f x = sequence (extend f (annotate (const ()) x))
