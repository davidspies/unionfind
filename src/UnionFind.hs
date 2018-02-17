module UnionFind
    ( UnionFind
    , find
    , start
    , union
    ) where

import           UnionFind.Internal (UnionFind, start)
import qualified UnionFind.Internal as UF
import           UnionFind.Node

find :: Int -> UnionFind -> Int
find x = unNode . UF.find (Node x)

union :: Int -> Int -> UnionFind -> UnionFind
union x y = UF.union (Node x) (Node y)
