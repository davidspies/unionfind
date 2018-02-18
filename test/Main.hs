module Main where

import           Control.Monad        (replicateM)
import           Control.Monad.State  (StateT, evalStateT)
import qualified Control.Monad.State  as State
import           Data.List            (foldl')
import           Graph                (Edge)
import qualified Graph
import           Test.Hspec
import           Test.QuickCheck
import           UnionFind
import           UnionFind.Introspect

ufFrom :: [Edge] -> UnionFind
ufFrom = foldl' (flip union) start

main :: IO ()
main = hspec $
  describe "UnionFind" $ do
    it "should tell if two nodes in a graph are connected" $
      property propMatchesGraphConnectivity
    it "should bound the indirection depth at each level" $
      property propBoundedIndirections
    it "should bound the indirection depth at each level in the worst case" $
      property propWorstCaseBounded

propMatchesGraphConnectivity :: EdgeList -> NPairs -> Property
propMatchesGraphConnectivity (EdgeList el) (NPairs nprs) =
  let g = Graph.fromEdges el
      uf = ufFrom el
      joined = [Graph.isConnected x y g | (x, y) <- nprs]
  in
  cover (or joined) 50 "any joined" $
  cover (any not joined) 50 "any not joined" $
  map (`isConnectedIn` uf) nprs === joined

twoPows :: [Int]
twoPows = iterate (2 *) 1

propBoundedIndirections :: EdgeList -> Property
propBoundedIndirections (EdgeList el) =
  let uf = ufFrom el
      is = indirectionDepth uf
  in
  counterexample
    (show is ++ " not below " ++ show (take (length is) twoPows)) $
  and $ zipWith (<=) is twoPows

worstCase :: [(Int, Int)]
worstCase = (2, 1) : concatMap nextWorstCase (iterate (2 *) 2)
  where
    nextWorstCase :: Int -> [(Int, Int)]
    nextWorstCase n = shift n (take (n - 1) worstCase) ++ [(n + 1, 1)]
    shift :: Int -> [(Int, Int)] -> [(Int, Int)]
    shift k = map (\(x, y) -> (x + k, y + k))

propWorstCaseBounded :: Int -> Property
propWorstCaseBounded n = propBoundedIndirections (EdgeList (take n worstCase))

newtype EdgeList = EdgeList [Edge]
  deriving (Show)

instance Arbitrary EdgeList where
  arbitrary = sized $ \size -> do
    n <- choose (0, size)
    let
      pickNextEdge :: StateT UnionFind Gen (Int, Int)
      pickNextEdge = do
        uf <- State.get
        pr <- State.lift (arbitrary `suchThat` (not . (`isConnectedIn` uf)))
        State.put $ union pr uf
        return pr
    res <- evalStateT (replicateM n pickNextEdge) start
    return $ EdgeList res
  shrink (EdgeList el) = EdgeList <$> shrink el

newtype NPairs = NPairs [(Int, Int)]
  deriving (Show)

instance Arbitrary NPairs where
  arbitrary = fmap NPairs $ sized $ \n ->
    vectorOf (5 * n) (arbitrary `suchThat` uncurry (/=))
  shrink (NPairs l) = NPairs <$> shrink l
