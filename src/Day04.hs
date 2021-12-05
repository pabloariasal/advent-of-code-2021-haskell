module Day04 (solve) where

import Data.List (transpose)
import Data.List.Split (splitOn)

type Board = [[Int]]

part1 :: String -> String
part1 = show . uncurry play . parse

parse :: String -> ([Int], [Board])
parse s = (parseList (head parts), parseBoards (tail parts))
  where
    parts = lines s

parseList :: String -> [Int]
parseList = map read . splitOn ","

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards ("" : r1 : r2 : r3 : r4 : r5 : xs) = map createRow [r1, r2, r3, r4, r5] : parseBoards xs
  where
    createRow = map read . words
parseBoards _ = error "malformed input"

play :: [Int] -> [Board] -> Int
play [] _ = error "nobody won"
play (x : xs) bs = case filter isFinished updated of
  [] -> play xs updated
  (b : _) -> computeScore x b
  where
    updated = map (markNumber x) bs

markNumber :: Int -> Board -> Board
markNumber n = map (map f)
  where
    f i = if i == n then -1 else i

isFinished :: Board -> Bool
isFinished b = any (all isMarked) b || any (all isMarked) (transpose b)
  where
    isMarked = (== (-1))

computeScore :: Int -> Board -> Int
computeScore n b = n * sum (map rs b)
  where
    rs = foldr (\e a -> if e == -1 then a else e + a) 0

part2 :: String -> String
part2 = show . uncurry play2 . parse

play2 :: [Int] -> [Board] -> Int
play2 = f 0
  where
    f s _ [] = s
    f s [] _ = s
    f s (x : xs) bs = case won of
      [] -> f s xs notwon
      (b : _) -> f (computeScore x b) xs notwon
      where
        won = filter isFinished updated
        notwon = filter (not . isFinished) updated
        updated = map (markNumber x) bs

solve :: String -> IO ()
solve input = putStrLn "--- Day 04 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
