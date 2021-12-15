module Day08 (solve, part1, part2) where

import Data.List (find, sort)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M

part1 :: String -> String
part1 = show . sum . map (length . filter (`elem` [1, 4, 7, 8]) . uncurry decode) . parse

decode :: [String] -> [String] -> [Int]
decode input = map query
  where
    query e = case M.lookup (sort e) m of
      Just x -> x
      Nothing -> error ("number not decoded " ++ e)
    m = identify input

part2 :: String -> String
part2 = show . sum . map (toDecimal . uncurry decode) . parse

toDecimal :: [Int] -> Int
toDecimal l = sum $ zipWith (*) l [1000, 100, 10, 1]

parse :: String -> [([String], [String])]
parse = map parseLine . lines

parseLine :: String -> ([String], [String])
parseLine s =
  let parts = splitOn "|" s
   in (words $ head parts, words $ last parts)

identify :: [String] -> Map String Int
identify l = M.fromList $ zip (map sort [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9]) [0 ..]
  where
    d1 = f (\x -> length x == 2)
    d4 = f (\x -> length x == 4)
    d8 = f (\x -> length x == 7)
    d7 = f (\x -> length x == 3)
    d2 = f (\x -> length x == 5 && overlaps x d4 2)
    d3 = f (\x -> length x == 5 && overlaps x d4 3 && contains x d1)
    d5 = f (\x -> length x == 5 && overlaps x d4 3 && not (contains x d1))
    d6 = f (\x -> length x == 6 && not (contains x d1))
    d9 = f (\x -> length x == 6 && contains x d3)
    d0 = f (\x -> length x == 6 && not (contains x d3) && contains x d1)
    f p = case find p l of
      Just x -> x
      Nothing -> error "logic error"
    contains a = all (`elem` a)
    overlaps a b n = let bina = filter (`elem` a) b in length bina == n

solve :: String -> IO ()
solve input = putStrLn "--- Day 08 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
