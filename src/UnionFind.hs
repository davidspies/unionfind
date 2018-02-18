module UnionFind
    ( UnionFind
    , find
    , isConnectedIn
    , start
    , union
    , unionIn
    ) where

import qualified UnionFind.Internal as UF
import           UnionFind.Node
import           UnionFind.Wrapper

find :: Int -> UnionFind -> Int
find x = unNode . UF.find (Node x) . unuf

union, unionIn :: (Int, Int) -> UnionFind -> UnionFind
union (x, y) = UnionFind . UF.union (Node x, Node y) . unuf
unionIn = union

isConnectedIn :: (Int, Int) -> UnionFind -> Bool
isConnectedIn (x, y) uf = find x uf == find y uf

start :: UnionFind
start = UnionFind UF.start
