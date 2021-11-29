module Day02 (solve) where

part1 :: String -> String
part1 _ = "Day 2 part 1"

part2 :: String -> String
part2 s = "Day 2 part 2" ++ s

solve :: String -> IO ()
solve input = putStrLn "--- Day 02 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
