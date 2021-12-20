module Day09 (solve, part1, part2) where

import Data.Char (digitToInt)
import Data.List (sort)
import Mat

part1 :: String -> String
part1 s = show $ sum (map ((+ 1) . value m) (getMinima m))
  where
    m = fromString digitToInt s

getMinima :: Mat Int -> [Coord]
getMinima m = filter (isLocalMin m) allCoords
  where
    allCoords = [(r, c) | r <- [0 .. (h - 1)], c <- [0 .. (w - 1)]]
    h = height m
    w = width m

isLocalMin :: Mat Int -> Coord -> Bool
isLocalMin m c = all (> e) neighboringValues
  where
    e = value m c
    neighboringValues = map (value m) neighbors
    neighbors = neighboringCells m c

part2 :: String -> String
part2 s = show . product . take 3 . reverse . sort . map length $ basins
  where
    basins = map (basin m []) minima
    minima = getMinima m
    m = fromString digitToInt s

basin :: Mat Int -> [Coord] -> Coord -> [Coord]
basin m v c =
  if (c `notElem` v) && value m c < 9
    then foldl (basin m) (c : v) (neighboringCells m c)
    else v

solve :: String -> IO ()
solve input = putStrLn "--- Day 09 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
