{-# LANGUAGE RecordWildCards #-}

module UnionFind.Introspect
    ( indirectionDepth
    ) where

import           UnionFind.Internal
import qualified UnionFind.Single   as S

indirectionDepth :: UnionFind -> [Int]
indirectionDepth UnionFind{..} =
  map S.indirectionDepth $
    S.getCompressed front : map fst intermediates ++ [back]
