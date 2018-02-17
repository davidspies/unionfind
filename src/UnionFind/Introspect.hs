module UnionFind.Introspect
    ( indirectionDepth
    ) where

import           UnionFind.Internal
import qualified UnionFind.Single   as S

indirectionDepth :: UnionFind -> [Int]
indirectionDepth (UnionFind x xs) =
  map S.indirectionDepth (S.getCompressed x : map fst xs)
