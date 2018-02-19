module Graph
    ( Edge
    , Graph
    , fromEdges
    , isConnectedIn
    ) where

import Control.Monad.ST (ST, runST)
import Data.Bifunctor (second)
import qualified Data.DList as DList
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)
import Data.Tuple (swap)

newtype Graph = Graph (IntMap [Int])
  deriving (Show)

type Node = Int
type Edge = (Node, Node)

fromEdges :: [Edge] -> Graph
fromEdges edges = Graph $ IntMap.map DList.toList $ IntMap.fromListWith (<>) $
  map (second DList.singleton) $ edges ++ map swap edges

neighbors :: Graph -> Node -> [Node]
neighbors (Graph m) n = fromMaybe [] $ IntMap.lookup n m

isConnectedIn :: (Node, Node) -> Graph -> Bool
isConnectedIn (x0, y) g = runST (newSTRef IntSet.empty >>= go x0)
  where
    go :: Node -> STRef s IntSet -> ST s Bool
    go x visited
      | x == y = return True
      | otherwise = IntSet.member x <$> readSTRef visited >>= \case
          True -> return False
          False -> do
            modifySTRef visited (IntSet.insert x)
            or <$> mapM (`go` visited) (neighbors g x)
