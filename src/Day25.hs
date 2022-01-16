module Day25 (solve, part1) where

import Data.List (transpose)
import Parsing
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char (char, space)

data Cell = Empty | East | South deriving (Eq, Show)

part1 :: String -> String
part1 = show . run . parse

parse :: String -> [[Cell]]
parse = parseWith (many line)
  where
    line :: Parser [Cell]
    line = some cell <* space
    cell :: Parser Cell
    cell = Empty <$ char '.' <|> East <$ char '>' <|> South <$ char 'v'

switch :: [[Cell]] -> [[Cell]]
switch = (map . map) f
  where
    f :: Cell -> Cell
    f East = South
    f South = East
    f Empty = Empty

move :: [Cell] -> [Cell]
move row = zipWith3 moveEast (last row : init row) row (tail row ++ [head row])
  where
    moveEast East Empty _ = East
    moveEast _ East Empty = Empty
    moveEast _ x _ = x

step :: [[Cell]] -> [[Cell]]
step = switch . transpose . map move . switch . transpose . map move

run :: [[Cell]] -> Int
run = go 1
  where
    go n current = let new = step current in if new == current then n else go (n + 1) new

solve :: String -> IO ()
solve input = putStrLn "--- Day 25 ---" >> putStrLn (part1 input)
