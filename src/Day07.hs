module Day07 (solve) where

import Data.List.Split (splitOn)

-- this is saddly a brute force solution, as I couldn't really think of anything better
-- surfing the internet however made me realize that this is just the mean and median😅
part1 :: String -> String
part1 i = show $ minimum (map m input)
  where
    input = parse i
    m = flip computeFuelCost input

part2 :: String -> String
part2 i = show $ minimum (map m input)
  where
    input = parse i
    m = flip computeFuelCost2 input

parse :: String -> [Int]
parse i = map read (splitOn "," i)

computeFuelCost :: Int -> [Int] -> Int
computeFuelCost n = sum . differences n

computeFuelCost2 :: Int -> [Int] -> Int
computeFuelCost2 n l = sum $ [sum [1 .. d] | d <- differences n l]

differences :: Int -> [Int] -> [Int]
differences n = map (abs . (-) n)

solve :: String -> IO ()
solve input = putStrLn "--- Day 07 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
