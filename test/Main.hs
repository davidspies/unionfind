module Main where

import           Control.Monad   (replicateM)
import           Data.List       (foldl')
import           Graph           (Edge)
import qualified Graph
import           Test.Hspec
import           Test.QuickCheck
import           UnionFind

ufFrom :: [Edge] -> UnionFind
ufFrom = foldl' (\uf (x, y) -> union x y uf) start

main :: IO ()
main = hspec $
  describe "UnionFind" $
    it "should tell if two nodes in a graph are connected" $
      property propMatchesGraphConnectivity

propMatchesGraphConnectivity :: [Edge] -> NPairs -> Property
propMatchesGraphConnectivity el (NPairs nprs) =
  let g = Graph.fromEdges el
      uf = ufFrom el
      joined = [Graph.isConnected x y g | (x, y) <- nprs]
  in
  cover (or joined) 50 "any joined" $
  cover (any not joined) 50 "any not joined" $
  map (\(x, y) -> find x uf == find y uf) nprs === joined

newtype NPairs = NPairs [(Int, Int)]
  deriving (Show)

instance Arbitrary NPairs where
  arbitrary = fmap NPairs $ sized $ \n ->
    replicateM (5 * n) (arbitrary `suchThat` uncurry (/=))
  shrink (NPairs l) = NPairs <$> shrink l
