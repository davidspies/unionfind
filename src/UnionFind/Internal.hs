{-# LANGUAGE RecordWildCards #-}

module UnionFind.Internal
    ( Builder (..)
    , UnionFind (..)
    , union
    , find
    , start
    ) where

import Data.Bifunctor (first)

import UnionFind.Node (Node)
import qualified UnionFind.Single as S

data Bit = Zero | One
  deriving (Eq, Show)
data Builder = Builder
  { intermediates :: [(S.UnionFind, Bit)]
  , back          :: S.UnionFind
  , counter       :: EdgeCounter
  }
  deriving (Eq, Show)
data EdgeCounter = EdgeCounter
  { edgeCount  :: !Integer
  , nextExpand :: !Integer
  }
  deriving (Eq, Show)
data UnionFind = UnionFind S.Compressed Builder
  deriving (Eq, Show)

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
union (x, y) uf@(UnionFind front Builder{..})
  | S.captainNode xcapt == S.captainNode ycapt = uf
  | otherwise =
    takeStep Builder
      { intermediates=map (first addEdge) intermediates
      , back=addEdge back
      , counter=increment counter
      }
  where
    xcapt = S.find x front
    ycapt = S.find y front
    addEdge = S.union (xcapt, ycapt)

find :: Node -> UnionFind -> Node
find n (UnionFind front _) = S.captainNode $ S.find n front

takeStep :: Builder -> UnionFind
takeStep Builder{..} = case intermediates of
  [] ->
    let
      nextFront = S.compress back
      (nextCounter, wasReset) = tryReset counter
    in UnionFind nextFront Builder
      { intermediates=[(S.getCompressed nextFront, Zero) | wasReset]
      , back
      , counter=nextCounter
      }
  ((x, b) : xs) ->
    let (nextIntermediates, nextCounter) = case b of
          Zero -> ((x, One) : xs, counter)
          One ->
            let UnionFind y Builder{intermediates=ys, counter=yctr} =
                  takeStep Builder{intermediates=xs, ..}
            in ((S.getCompressed y, Zero) : ys, yctr)
    in UnionFind (S.compress x) Builder
      { intermediates=nextIntermediates
      , back
      , counter=nextCounter
      }

start :: UnionFind
start = UnionFind S.start Builder
  { intermediates=[]
  , back=S.getCompressed S.start
  , counter=newCounter
  }
