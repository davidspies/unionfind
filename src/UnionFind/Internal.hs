{-# LANGUAGE RecordWildCards #-}

module UnionFind.Internal
    ( UnionFind
    , UnionFind'(..)
    , union
    , find
    , start
    ) where

import           Data.Bifunctor   (first)
import           UnionFind.Node   (Node)
import qualified UnionFind.Single as S

data Bit = Zero | One
data UnionFind' front = UnionFind
  { front         :: front
  , intermediates :: [(S.UnionFind, Bit)]
  , back          :: S.UnionFind
  , counter       :: EdgeCounter
  }
data EdgeCounter = EdgeCounter
  { edgeCount  :: Integer
  , nextExpand :: Integer
  }
type UnionFind = UnionFind' S.Compressed
type Builder = UnionFind' S.UnionFind

newCounter :: EdgeCounter
newCounter = EdgeCounter{edgeCount=0, nextExpand=4}

increment :: EdgeCounter -> EdgeCounter
increment EdgeCounter{..} = EdgeCounter{edgeCount=edgeCount + 1, nextExpand}

tryReset :: EdgeCounter -> (EdgeCounter, Bool)
tryReset ec@EdgeCounter{..} =
  if edgeCount >= nextExpand
  then
    ( EdgeCounter
      { edgeCount
      , nextExpand=nextExpand ^ (2 :: Int)
      }
    , True
    )
  else (ec, False)

union :: (Node, Node) -> UnionFind -> UnionFind
union (x, y) uf@UnionFind{..}
  | S.captainNode xcapt == S.captainNode ycapt = uf
  | otherwise =
    takeStep UnionFind
      { front=addEdge $ S.getCompressed front
      , intermediates=map (first addEdge) intermediates
      , back=addEdge back
      , counter=increment counter
      }
  where
    xcapt = S.find x front
    ycapt = S.find y front
    addEdge = S.union (xcapt, ycapt)

find :: Node -> UnionFind -> Node
find n UnionFind{front} = S.captainNode $ S.find n front

takeStep :: Builder -> UnionFind
takeStep UnionFind{..} = case intermediates of
  [] ->
    let
      nextFront = S.compress back
      (nextCounter, wasReset) = tryReset counter
    in UnionFind
      { front=nextFront
      , intermediates=[(S.getCompressed nextFront, Zero) | wasReset]
      , back
      , counter=nextCounter
      }
  ((x, b) : xs) ->
    let (nextIntermediates, nextCounter) = case b of
          Zero -> ((x, One) : xs, counter)
          One ->
            let UnionFind{front=y, intermediates=ys, counter=yctr} =
                  takeStep UnionFind{front=x, intermediates=xs, ..}
            in ((S.getCompressed y, Zero) : ys, yctr)
    in UnionFind
      { front=S.compress x
      , intermediates=nextIntermediates
      , back
      , counter=nextCounter
      }

start :: UnionFind
start = UnionFind
  { front=S.start
  , intermediates=[]
  , back=S.getCompressed S.start
  , counter=newCounter
  }
