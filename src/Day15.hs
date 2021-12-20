module Day15 (solve, part1, part2) where

import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)
import Dijkstra (findPath)
import Mat (Mat)
import qualified Mat as M

edges :: Mat Int -> M.Coord -> [(M.Coord, Int)]
edges m c =
  let neighbors = M.neighboringCells m c
   in map (\c -> (c, M.value m c)) neighbors

lastCoord :: Mat a -> M.Coord
lastCoord m = (M.height m - 1, M.width m - 1)

run :: Mat Int -> String
run m = show . snd . last $ findPath (0, 0) (lastCoord m) (edges m)

part1 :: String -> String
part1 = run . M.fromString digitToInt

part2 :: String -> String
part2 = run . parseLargeCave

parseLargeCave :: String -> Mat Int
parseLargeCave = M.from2DList . concat . take 5 . iterate (map increase) . map toRow . lines
  where
      toRow = concat . take 5 . iterate increase . map digitToInt
      increase = map $ \x -> (x `mod` 9) + 1

solve :: String -> IO ()
solve input = putStrLn "--- Day 15 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
