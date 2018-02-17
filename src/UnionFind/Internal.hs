module UnionFind.Internal
    ( UnionFind(..)
    , union
    , find
    , start
    ) where

import           Data.Bifunctor   (first)
import           UnionFind.Node   (Node)
import qualified UnionFind.Single as S

data Bit = Zero | One
data UnionFind = UnionFind S.Compressed [(S.UnionFind, Bit)]

union :: (Node, Node) -> UnionFind -> UnionFind
union (x, y) uf@(UnionFind c ufs)
  | S.captainNode xcapt == S.captainNode ycapt = uf
  | otherwise = takeStep (addEdge $ S.getCompressed c) (map (first addEdge) ufs)
  where
    xcapt = S.find x c
    ycapt = S.find y c
    addEdge = S.union (xcapt, ycapt)

find :: Node -> UnionFind -> Node
find n (UnionFind c _) = S.captainNode $ S.find n c

takeStep :: S.UnionFind -> [(S.UnionFind, Bit)] -> UnionFind
takeStep uf []           = UnionFind (S.compress uf) [(uf, One)]
takeStep _ ((x, b) : xs) = UnionFind (S.compress x) $ case b of
  Zero -> (x, One) : xs
  One  -> (S.getCompressed y, Zero) : ys
    where
      UnionFind y ys = takeStep x xs

start :: UnionFind
start = UnionFind S.start []
