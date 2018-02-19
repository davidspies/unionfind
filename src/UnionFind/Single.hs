module UnionFind.Single
    ( UnionFind
    , CaptainNode
    , Compressed
    , Rank
    , captainNode
    , compress
    , indirectionDepth
    , find
    , getCompressed
    , start
    , union
    ) where

import Data.Maybe (fromJust, fromMaybe)

import UnionFind.Node (Node, NodeMap)
import qualified UnionFind.Node as Node

type Rank = Int
data Value v = Pointer !v | Captain !Rank
  deriving (Eq, Functor, Show)
newtype UnionFind' v = UnionFind (NodeMap (Value v))
  deriving (Eq, Functor, Show)
type UnionFind = UnionFind' Node
type Compressed = UnionFind' CaptainNode
data CaptainNode = CaptainNode !Node !Rank
  deriving (Eq, Show)

getCompressed :: Compressed -> UnionFind
getCompressed uf = captainNode <$> uf

(!) :: UnionFind' v -> Node -> Value v
(!) (UnionFind m) k = fromMaybe (Captain 0) $ Node.lookup k m

find :: Node -> Compressed -> CaptainNode
find n uf = case uf ! n of
  Pointer p -> p
  Captain r -> CaptainNode n r

captainNode :: CaptainNode -> Node
captainNode (CaptainNode n _) = n

union :: (CaptainNode, CaptainNode) -> UnionFind -> UnionFind
union (CaptainNode x xr, CaptainNode y yr) (UnionFind m) = UnionFind $
  case compare xr yr of
    LT -> Node.insert x (Pointer y) m
    EQ -> Node.insertAll [(x, Pointer y), (y, Captain (yr + 1))] m
    GT -> Node.insert y (Pointer x) m

compress :: UnionFind -> Compressed
compress (UnionFind m) = res
  where
    res = UnionFind $ pathCompress <$> m
    pathCompress = \case
      Captain r -> Captain r
      Pointer p -> Pointer (find p res)

start :: Compressed
start = UnionFind Node.empty

indirectionDepth :: UnionFind -> Int
indirectionDepth (UnionFind m) = maximum $ 0 : Node.elems stepCount
  where
    stepCount :: NodeMap Int
    stepCount = go <$> m
    go :: Value Node -> Int
    go = \case
      Captain{} -> 0
      Pointer p -> fromJust (Node.lookup p stepCount) + 1
