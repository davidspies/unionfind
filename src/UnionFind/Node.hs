module UnionFind.Node
    ( Node(..)
    , NodeMap
    , elems
    , empty
    , insert
    , insertAll
    , lookup
    , unNode
    ) where

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import           Data.List        (foldl')
import           Prelude          hiding (lookup)

newtype Node = Node Int
  deriving (Eq)
newtype NodeMap a = NodeMap (IntMap a)
  deriving (Functor)

unNode :: Node -> Int
unNode (Node n) = n

insert :: Node -> a -> NodeMap a -> NodeMap a
insert (Node k) v (NodeMap m) = NodeMap $ IntMap.insert k v m

empty :: NodeMap a
empty = NodeMap IntMap.empty

lookup :: Node -> NodeMap a -> Maybe a
lookup (Node n) (NodeMap m) = IntMap.lookup n m

insertAll :: [(Node, a)] -> NodeMap a -> NodeMap a
insertAll xs m = foldl' (flip (uncurry insert)) m xs

elems :: NodeMap a -> [a]
elems (NodeMap m) = IntMap.elems m
