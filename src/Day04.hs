module Day04 (solve) where

import Data.List.Split (splitOn)
import Data.List (transpose)

type Board = [[Int]]

part1 :: String -> String
part1 s =
  let parts = lines s
      nums = parseList (head parts)
      boards = parseBoards (tail parts)
   in show $ play nums boards

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
isFinished b = any (all (==(-1))) b || any (all (==(-1))) (transpose b)

computeScore :: Int -> Board -> Int
computeScore n b = n * sum (map rs b)
  where rs = foldr (\e a -> if e == -1 then a else e + a) 0

part2 :: String -> String
part2 _ = "Not implemented"

solve :: String -> IO ()
solve input = putStrLn "--- Day 04 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
