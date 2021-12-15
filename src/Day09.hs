module Day09 (solve, part1, part2) where

import Data.Char (digitToInt)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V

type Coord = (Int, Int)

data Mat = Mat {values :: V.Vector Int, width :: Int, height :: Int} deriving (Eq, Show)

part1 :: String -> String
part1 s = show $ sum (map ((+ 1) . val m) (getMinima m))
  where
    m = parse s

getMinima :: Mat -> [Coord]
getMinima m = filter (isLocalMin m) allCoords
  where
    allCoords = [(r, c) | r <- [0 .. (h - 1)], c <- [0 .. (w - 1)]]
    h = height m
    w = width m

parse :: String -> Mat
parse s = Mat d width height
  where
    d = V.fromList $ concatMap parseLine (lines s)
    height = V.length d `div` width
    width = length (head $ lines s)
    parseLine = map digitToInt

isLocalMin :: Mat -> Coord -> Bool
isLocalMin m c = all (> e) neighboringValues
  where
    e = val m c
    neighboringValues = map (val m) neighbors
    neighbors = neighboringCells m c

neighboringCells :: Mat -> Coord -> [Coord]
neighboringCells m (r, c) = mapMaybe coord n
  where
    n = [(r + 1, c), (r - 1, c), (r, c - 1), (r, c + 1)]
    coord x = if inside x then Just x else Nothing
    inside (nr, nc) = nr >= 0 && nr < h && nc >= 0 && nc < w
    w = width m
    h = height m

val :: Mat -> Coord -> Int
val m (r, c) = values m V.! i
  where
    i = r * width m + c

part2 :: String -> String
part2 s = show . product . take 3 . reverse . sort . map length $ basins
  where
    basins = map (basin m []) minima
    minima = getMinima m
    m = parse s

basin :: Mat -> [Coord] -> Coord -> [Coord]
basin m v c =
  if (c `notElem` v) && val m c < 9
    then foldl (basin m) (c : v) (neighboringCells m c)
    else v

solve :: String -> IO ()
solve input = putStrLn "--- Day 09 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)