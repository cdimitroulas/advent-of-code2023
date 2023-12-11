{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib.SearchAlgorithms (DijkstraGraph(..), Distance(..), findShortestDistance) where

import Data.Heap (MinPrioHeap)
import qualified Data.Heap as H
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Kind (Type)
import Data.Foldable (foldl')

data Distance a = Dist a | Infinity
  deriving (Show, Eq)

instance (Ord a) => Ord (Distance a) where
  Infinity <= Infinity = True
  Infinity <= Dist _ = False
  Dist _ <= Infinity = True
  Dist x <= Dist y = x <= y

addDist :: (Num a) => Distance a -> Distance a -> Distance a
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

(!??) :: (Hashable k) => HashMap k (Distance d) -> k -> Distance d
(!??) distanceMap key = fromMaybe Infinity (HM.lookup key distanceMap)

class DijkstraGraph graph where
  type DijkstraNode graph :: Type
  type DijkstraCost graph :: Type
  dijkstraEdges :: graph -> DijkstraNode graph -> [(DijkstraNode graph, DijkstraCost graph)]

              
data DijkstraState node cost = DijkstraState
  { visitedSet :: HashSet node
  , distanceMap :: HashMap node (Distance cost)
  , nodeQueue :: MinPrioHeap (Distance cost) node
  }

-- Taken from https://mmhaskell.com/blog/2022/8/22/dijkstras-algorithm-in-haskell
findShortestDistance :: 
  forall g. (Hashable (DijkstraNode g), Num (DijkstraCost g), Ord (DijkstraCost g), DijkstraGraph g) => g -> DijkstraNode g -> DijkstraNode g -> Distance (DijkstraCost g)
findShortestDistance graph src dest = processQueue initialState !?? dest
  where
    initialVisited = HS.empty
    initialDistances = HM.singleton src (Dist 0)
    initialQueue = H.fromList [(Dist 0, src)]
    initialState = DijkstraState initialVisited initialDistances initialQueue

    processQueue ::   DijkstraState (DijkstraNode g) (DijkstraCost g) -> HashMap (DijkstraNode g) (Distance (DijkstraCost g))
    processQueue ds@(DijkstraState v0 d0 q0 ) = case H.view q0 of
          -- if there is nothing left in the queue, we have processed everything and we can just return the distances
          Nothing -> d0
          Just ((_, node), q1) -> 
            -- if we have reached the destination node, we have processed everything
            if node == dest 
            then d0 
            -- if the node is already visited, then we can immediately recurse, except plugging in the updated queue
            else if HS.member node v0 then processQueue ds{ nodeQueue = q1}
            -- 1. Pull a new node from our heap and consider that node “visited”
            -- 2. Get all the “neighbors” of this node
            -- 3. Process each neighbor and update its distance
            else 
              let v1 = HS.insert node v0
                  allNeighbors = dijkstraEdges graph node
                  unvisitedNeighbors = filter (\(n, _) -> not (HS.member n v1)) allNeighbors
              in processQueue $ foldl' (foldNeighbor node) ds{ visitedSet=v1, nodeQueue=q1 } unvisitedNeighbors

    -- Take the distance from the source to the current node and add it to the specific edge cost from the current to this new node.
    -- Then compare this distance to the existing distance we have to the neighbor in our distance map (or Infinity if it doesn’t exist).
    foldNeighbor currentNode ds@(DijkstraState _ d0 q0) (neighborNode, cost) =
      let altDistance = addDist (d0 !?? currentNode) (Dist cost)
      in  if altDistance < d0 !?? neighborNode
          -- If the alternative distance is smaller, we update the distance map by associating the neighbor node with the 
          -- alternative distance and return the new DijkstraState. We also insert the new distance into our queue. 
          then ds { distanceMap = HM.insert neighborNode altDistance d0, nodeQueue = H.insert (altDistance, neighborNode) q0 }
          -- If the alternative distance is not better, we make no changes, and return the original state.
          else ds
