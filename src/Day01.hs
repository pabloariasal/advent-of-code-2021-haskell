module Day01 (solve) where

part1 :: [Int] -> Int
part1 [] = 0
part1 [_] = 0
part1 (x:y:xs) = if y > x then 1 + part1 (y : xs) else part1(y : xs)

part2 :: [Int] -> Int
part2 = part1 . s
    where s :: [Int] -> [Int]
          s (x:y:z:xs) = (x + y + z) : s (y : z : xs)
          s _ = []

solve :: String -> IO ()
solve input = putStrLn "--- Day 01 ---" >> print (part1 $ p input) >> print (part2 $ p input)
    where p = map read . lines
