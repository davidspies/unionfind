module UnionFind.Wrapper where

import qualified UnionFind.Internal as UF

newtype UnionFind = UnionFind {unuf :: UF.UnionFind}
