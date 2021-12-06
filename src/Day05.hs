module Day05 (solve) where

import Data.List (group, sort)
import Data.List.Split

type Point = (Int, Int)
type Line = (Point, Point)

part1 :: String -> String
part1 = show . countOverlaps 1 . concatMap extractPoints . removeDiagonal . parse

part2 :: String -> String
part2 = show . countOverlaps 1 . concatMap extractPoints . parse

parse :: String -> [Line]
parse = map (p . splitOn " -> ") . lines
    where p [p1,p2] = (s $ splitOn "," p1, s $ splitOn "," p2)
          p _ = error "malformed input"
          s [x, y] = (read x, read y)
          s _ = error "malformed input"

removeDiagonal :: [Line] -> [Line]
removeDiagonal = filter (not . isDiagonal)
    where isDiagonal ((x1, y1), (x2, y2)) = (x1 /= x2) && (y2 /= y1)

extractPoints :: Line -> [Point]
extractPoints ((x1, y1), (x2, y2))
    | x1 == x2 = [(x1, y) | y <- y1 ... y2]
    | y1 == y2 = [(x, y1) | x <- x1 ... x2]
    | otherwise = zip (x1...x2) (y1...y2)

countOverlaps :: Int -> [Point] -> Int
countOverlaps n = length . filter ((>n) . length) . group . sort

-- I got some help from reddit for this neat idea 😅
(...) :: Int -> Int -> [Int]
x ... y
    | x <= y = [x..y]
    | otherwise = reverse [y..x]

solve :: String -> IO ()
solve input = putStrLn "--- Day 05 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
