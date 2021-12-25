module Day17 (solve, part1, part2) where

import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import Parsing

-- we want to have the lowest possible value at y=0 (would imply that we reached the highest y coord)
-- in order to hit the target in the lower edge, the y velocity must be -((abs target_min_y) - 1)
-- in order for the y velocity to have a value x at y=0, the initial velocity must be - x
-- the highest point is the triangular number of the y velocity
-- the formula is n(n + 1) / 2

part1 :: String -> String
part1 s =
  let (Target _ _ _ minY) = parse s
   in show $ triangular minY

part2 :: String -> String
part2 s = show . length . filter (== True) $ do
  vy <- [vyMin .. vyMax]
  vx <- [vxMin .. vxMax]
  return $ launchProbe target vx vy
  where
    (vyMin, vyMax) = findPossibleYVelocities target
    (vxMin, vxMax) = findPossibleXVelocities target
    target = parse s

findPossibleYVelocities :: Target -> (Int, Int)
findPossibleYVelocities (Target _ _ _ minY) = (minY, abs minY - 1)

findPossibleXVelocities :: Target -> (Int, Int)
findPossibleXVelocities (Target maxX minX _ _) = (smallest, maxX)
  where
    smallest = head $ filter (\x -> triangular x >= minX) [0 ..]

triangular :: Int -> Int
triangular n = (n * (n + 1)) `div` 2

launchProbe :: Target -> Int -> Int -> Bool
launchProbe (Target maxX minX maxY minY) dx dy = head $ mapMaybe getResult $ iterate step (0, 0, dx, dy)
  where
    step (x, y, dx, dy) = (x + dx, y + dy, dx - signum dx, dy - 1)
    getResult (x, y, _, _)
      | x <= maxX && x >= minX && y <= maxY && y >= minY = Just True
      | x > maxX = Just False
      | y < minY = Just False
      | otherwise = Nothing

data Target = Target {maxX :: Int, minX :: Int, maxY :: Int, minY :: Int} deriving (Eq, Show)

parse :: String -> Target
parse = parseWith parseTarget
  where
    parseTarget :: Parser Target
    parseTarget = do
      symbol "target area: x="
      minx <- signedInteger
      symbol ".."
      maxx <- signedInteger
      symbol ", y="
      miny <- signedInteger
      symbol ".."
      maxy <- signedInteger
      return $ Target maxx minx maxy miny

solve :: String -> IO ()
solve input = putStrLn "--- Day 17 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
