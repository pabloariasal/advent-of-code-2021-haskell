module Day01 (solve) where

part1 :: String -> String
part1 _ = "Day 1 part 1"

part2 :: String -> String
part2 s = "Day 1 part 2" ++ s

solve :: String -> IO ()
solve input = putStrLn "--- Day 01 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
