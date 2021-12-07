module Day06 (solve) where

import Data.List.Split (splitOn)
import qualified Data.MultiSet as MultiSet

type Input = MultiSet.MultiSet Int

part1 :: String -> String
part1 = show . length . applyNtimes 80 simulateDay . parse

part2 :: String -> String
part2 = show . length . applyNtimes 256 simulateDay . parse

parse :: String -> Input
parse = MultiSet.fromList . map read . splitOn ","

applyNtimes :: Int -> (a -> a) -> a -> a
applyNtimes n f x = iterate f x !! n

simulateDay :: Input -> Input
simulateDay = MultiSet.concatMap (\x -> if x == 0 then [6, 8] else [x - 1])

solve :: String -> IO ()
solve input = putStrLn "--- Day 06 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
