module Day12 (solve, part1, part2) where

import Data.Char (isLower)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List.Split (splitOn)

type Graph = HashMap String [String]

type Visited = HashMap String Int

paths :: Graph -> (String -> Visited -> Bool) -> [[String]]
paths g stop = go "start" M.empty
  where
    go :: String -> Visited -> [[String]]
    go "end" _ = [["end"]]
    go c v
      | stop c v = []
      | isSmall = map (c :) $ concatMap (`go` addVisited) n
      | otherwise = map (c :) $ concatMap (`go` v) n
      where
        n = M.findWithDefault [] c g
        isSmall = all isLower c
        addVisited = case M.lookup c v of
          Nothing -> M.insert c 1 v
          Just x -> M.adjust (+ 1) c v

parse :: String -> Graph
parse = M.fromListWith (++) . concatMap (tuplify . splitOn "-") . lines
  where
    tuplify [x, y] = [(x, [y]), (y, [x])]
    tuplify _ = error "problem parsing input"

part1 :: String -> String
part1 = show . length . (`paths` (\c v -> M.findWithDefault 0 c v >= 1)) . parse

part2 :: String -> String
part2 = show . length . (`paths` shouldStop) . parse
  where
    shouldStop c v = M.findWithDefault 0 "start" v > 1 || not  (null(M.filter (>= 2) v)) && M.findWithDefault 0 c v > 0

solve :: String -> IO ()
solve input = putStrLn "--- Day 12 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
