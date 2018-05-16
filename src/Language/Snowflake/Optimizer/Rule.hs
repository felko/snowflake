{-# LANGUAGE DeriveFunctor #-}

module Language.Snowflake.Optimizer.Rule where

import Control.Applicative

import Data.List

data Rule a = Rule
    { _rulePattern :: Pattern a
    , _ruleResult  :: [a] }

type ID = Int

data InstrPattern
    = P_NOP
    | P_POP
    | P_NOT | P_AND | P_OR
    | P_ADD | P_SUB | P_MUL | P_DIV | P_POW
    | P_POS | P_NEG
    | P_LT | P_LE | P_EQ | P_NEQ | P_GE | P_GT
    | P_RETURN
    | P_IF
    | P_CALL        ID
    | P_BUILD_LIST  ID
    | P_STORE       ID
    | P_LOAD        ID
    | P_LOAD_CONST  ID
    | P_JUMP        ID
    | P_ITER        ID
    deriving (Eq, Show)

-- class Match p r | p -> r where
--     match :: p a -> [a] -> r a

data Pattern a
    = PureP a
    | ManyP (Pattern a)
    | OptionalP (Pattern a)
    | AlternativeP (Pattern a) (Pattern a)
    | SequenceP (Pattern a) (Pattern a)
    | NoneP
    deriving Functor

instance Applicative Pattern where
    pure = PureP
    NoneP <*> px = NoneP
    fx <*> NoneP = NoneP
    (PureP f) <*> (PureP x) = PureP (f x)
    (PureP f) <*> (ManyP x) = ManyP (fmap f x)
    (PureP f) <*> (OptionalP x) = OptionalP (fmap f x)
    (PureP f) <*> (AlternativeP l r) = AlternativeP (fmap f l) (fmap f r)
    (PureP f) <*> (SequenceP x y) = SequenceP (fmap f x) (fmap f y)
    (ManyP f) <*> px = ManyP (f <*> px)
    (OptionalP f) <*> px = OptionalP (f <*> px)
    (AlternativeP f g) <*> px = AlternativeP (f <*> px) (g <*> px)
    (SequenceP f g) <*> px = SequenceP (f <*> px) (g <*> px)

instance Alternative Pattern where
    empty = NoneP
    (<|>) = AlternativeP

optional :: Pattern a -> Pattern a
optional = OptionalP

data Result a
    = PureR a
    | ManyR [Result a]
    | OptionalR (Maybe (Result a))
    | AlternativeR (Either (Result a) (Result a))
    | SequenceR (Result a) (Result a)
    | NoneR

instance Functor Result where
    fmap f (PureR x) = PureR (f x)
    fmap f (ManyR xs) = ManyR (fmap (fmap f) xs)
    fmap f (AlternativeR (Left  x)) = AlternativeR (Left  $ fmap f x)
    fmap f (AlternativeR (Right x)) = AlternativeR (Right $ fmap f x)
    fmap f (SequenceR b a) = SequenceR (fmap f b) (fmap f a)
    fmap f NoneR = NoneR

instance Applicative Result where
    pure = PureR
    NoneR <*> rx = NoneR
    fx <*> NoneR = NoneR
    (PureR f) <*> (PureR x) = PureR (f x)
    (PureR f) <*> (ManyR xs) = ManyR (fmap (fmap f) xs)
    (PureR f) <*> (OptionalR Nothing)  = OptionalR Nothing
    (PureR f) <*> (OptionalR (Just x)) = OptionalR (Just $ fmap f x)
    (PureR f) <*> (AlternativeR (Left  x)) = AlternativeR (Left  $ fmap f x)
    (PureR f) <*> (AlternativeR (Right x)) = AlternativeR (Right $ fmap f x)
    (PureR f) <*> (SequenceR x y) = SequenceR (fmap f x) (fmap f y)
    (ManyR f) <*> rx = ManyR (fmap (<*> rx) f)
    (AlternativeR (Left  f)) <*> rx = AlternativeR (Left  $ f <*> rx)
    (AlternativeR (Right f)) <*> rx = AlternativeR (Right $ f <*> rx)
    (SequenceR f g) <*> rx = SequenceR (f <*> rx) (g <*> rx)

match :: Eq a => Pattern a -> [a] -> (Result a, [a])
match (PureP x)
match (PureP x) (y:ys)
    | x == y    = (PureR y, ys)
    | otherwise = NoneR
match (ManyP p) xs = ManyR (foldr )
match NoneP xs = NoneR
