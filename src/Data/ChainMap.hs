module Data.ChainMap where

import Prelude hiding (lookup)

import Control.Lens
import Control.Applicative ((<|>))

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate, findIndex)
import Data.Maybe (maybe)

newtype ChainMap k a = ChainMap [Map k a]
    deriving Eq

instance (Show k, Show a) => Show (ChainMap k a) where
    show (ChainMap ms) = '[' : intercalate "," (map showScope ms) ++ "]"
        where showScope scope = '{' : intercalate "," (map showAssoc (Map.toList scope)) ++ "}"
              showAssoc (k, v) = show k ++ ": " ++ show v

instance Ord k => Functor (ChainMap k) where
    fmap f (ChainMap ms) = ChainMap (fmap (fmap f) ms)

instance Ord k => Foldable (ChainMap k) where
    foldr f d (ChainMap ms) = foldr f d (mconcat ms)

-- instance Ord k => Traversable (Env k) where
--     traverse f e =

instance Ord k => Monoid (ChainMap k a) where
    mempty = ChainMap []
    mappend (ChainMap ms) (ChainMap ms') = undefined

lookup :: Ord k => k -> ChainMap k a -> Maybe a
lookup _ (ChainMap [])     = Nothing
lookup k (ChainMap (m:ms)) = Map.lookup k m <|> lookup k (ChainMap ms)

findWithDefault :: Ord k => a -> k -> ChainMap k a -> a
findWithDefault d k cm = maybe d id (lookup k cm)

update :: Ord k => k -> a -> ChainMap k a -> ChainMap k a
update k v (ChainMap []) = ChainMap [Map.singleton k v]
update k v cm@(ChainMap ms) = case mapIndex k cm of
    Just i  -> ChainMap (ms & ix i %~ Map.insert k v)
    Nothing -> ChainMap (ms & ix 0 %~ Map.insert k v)

insert :: Ord k => k -> a -> ChainMap k a -> ChainMap k a
insert k v (ChainMap []) = ChainMap [Map.singleton k v]
insert k v (ChainMap (m:ms)) = ChainMap (Map.insert k v m : ms)

member :: Ord k => k -> ChainMap k a -> Bool
member k (ChainMap ms) = any (Map.member k) ms

mapIndex :: Ord k => k -> ChainMap k a -> Maybe Int
mapIndex k (ChainMap ms) = findIndex (Map.member k) ms

newChild :: Ord k => Map k a -> ChainMap k a -> ChainMap k a
newChild m (ChainMap ms) = ChainMap (m:ms)

empty :: Ord k => ChainMap k a
empty = ChainMap []

singleMap :: Ord k => Map k a -> ChainMap k a
singleMap m = ChainMap [m]

fromMaps :: Ord k => [Map k a] -> ChainMap k a
fromMaps = ChainMap

fromLists :: Ord k => [[(k, a)]] -> ChainMap k a
fromLists l = ChainMap (map Map.fromList l)


m :: ChainMap Char Int
m = ChainMap [ Map.fromList [('a', 1), ('b', 2), ('c', 3)]
             , Map.fromList [('x', 3), ('b', 8), ('z', 9)]
             , Map.fromList [] ]
