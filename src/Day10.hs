module Day10 (solve, part1, part2) where

import Control.Monad (foldM)
import Data.Either (lefts, rights)
import Data.List (sort)

part1 :: String -> String
part1 = show . sum . map score . lefts . map parse . lines
  where
    score ')' = 3
    score ']' = 57
    score '}' = 1197
    score '>' = 25137

parse :: String -> Either Char [Char]
parse = foldM step []
  where
    step :: String -> Char -> Either Char [Char]
    step acc c = case c of
      '(' -> Right $ ')' : acc
      '<' -> Right $ '>' : acc
      '{' -> Right $ '}' : acc
      '[' -> Right $ ']' : acc
      _ -> if head acc == c then Right (tail acc) else Left c

part2 :: String -> String
part2 = show . middle . sort . map (foldl score 0) . rights . map parse . lines
  where
    score s ')' = s * 5 + 1
    score s ']' = s * 5 + 2
    score s '}' = s * 5 + 3
    score s '>' = s * 5 + 4
    middle l = l !! (length l `div` 2)

solve :: String -> IO ()
solve input = putStrLn "--- Day 10 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
