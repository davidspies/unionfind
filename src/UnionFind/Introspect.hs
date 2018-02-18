{-# LANGUAGE RecordWildCards #-}

module UnionFind.Introspect
    ( Structure
    , indirectionDepth
    , structureOf
    ) where

import           UnionFind.Internal
import qualified UnionFind.Single   as S
import qualified UnionFind.Wrapper  as W

indirectionDepth :: W.UnionFind -> [Int]
indirectionDepth (W.UnionFind (UnionFind front Builder{..})) =
  map S.indirectionDepth $
    S.getCompressed front : map fst intermediates ++ [back]

newtype Structure = Structure UnionFind
  deriving (Eq, Show)

structureOf :: W.UnionFind -> Structure
structureOf (W.UnionFind uf) = Structure uf
