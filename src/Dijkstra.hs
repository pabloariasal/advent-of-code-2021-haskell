module Dijkstra (findPath) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.PQueue.Min as Q

type EdgesFunc a = a -> [(a, Int)]

type PrevMap a = Map a (a, Int)

findPath :: Ord a => a -> a -> EdgesFunc a -> [(a, Int)]
findPath start end edges = assemblePath start end . dPrev $ until isDone step initialStep
  where
    initialStep = DState (Q.singleton start) (M.singleton start (start, 0)) edges end

isDone :: DState a -> Bool
isDone = Q.null . dQueue

assemblePath :: (Eq a, Ord a) => a -> a -> PrevMap a -> [(a, Int)]
assemblePath start end prevs = go end []
  where
    go current path
      | current == start = newPath
      | otherwise = go parent newPath
      where
        (parent, cost) = prevs M.! current
        newPath = (current, cost) : path

data DState a = DState
  { dQueue :: Q.MinQueue a,
    dPrev :: PrevMap a,
    dEdgesFunc :: EdgesFunc a,
    dEnd :: a
  }

step :: (Eq a, Ord a) => DState a -> DState a
step s@DState {..} =
  let (currentNode, newQueue) = Q.deleteFindMin dQueue
   in if currentNode == dEnd
        then s {dQueue = Q.empty}
        else foldr (update currentNode) s {dQueue = newQueue} $ dEdgesFunc currentNode

update :: Ord a => a -> (a, Int) -> DState a -> DState a
update current (next, distance) s@DState {..} =
  case M.lookup next dPrev of
    Nothing -> addNeighbor
    Just bestCostSoFar -> if newCost < snd bestCostSoFar then addNeighbor else s
  where
    addNeighbor =
      s
        { dQueue = Q.insert next dQueue,
          dPrev = M.insert next (current, newCost) dPrev
        }
    newCost = cost current + distance
    cost n = snd (dPrev M.! n)
