import Test.Hspec
import Test.QuickCheck

import Spec.Util

main :: IO ()
main = hspec $
  describe "UnionFind" $ do
    it "should tell if two nodes in a graph are connected" $
      property propMatchesGraphConnectivity
    it "should bound the indirection depth at each level" $
      property propBoundedIndirections
    it "should bound the indirection depth at each level in the worst case" $
      property propWorstCaseBounded
    it "should not change when joining two already-joined nodes" $
      property propNoStructuralChange
