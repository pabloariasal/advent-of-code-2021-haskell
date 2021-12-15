module Day13 (solve, part1, part2) where

import Control.Monad.State
import Data.Foldable (foldl')
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Parsing
import Text.Megaparsec hiding (State, parse)

type Coord = (Int, Int)

type Coords = HashSet Coord

data Fold = Vertical Int | Horizontal Int deriving (Eq, Show)

part1 :: String -> String
part1 s = let (coords, f : _) = parse s in show . length $ applyAll coords [f]

part2 :: String -> String
part2 s = let (coords, folds) = parse s in display $ applyAll coords folds

applyAll :: Coords -> [Fold] -> Coords
applyAll = foldl' apply

apply :: Coords -> Fold -> Coords
apply coords fold = S.map (applySingle fold) coords
  where
    applySingle f (x, y) = case f of
      (Horizontal dy) -> (x, dy - abs (y - dy))
      (Vertical dx) -> (dx - abs (x - dx), y)

display :: Coords -> String
display cs = unlines $ do
  y <- [minY .. maxY]
  return $ do
    x <- [minX .. maxX]
    return $ if (x, y) `S.member` cs then '#' else ' '
  where
    minX = minimum $ S.map fst cs
    maxX = maximum $ S.map fst cs
    minY = minimum $ S.map snd cs
    maxY = maximum $ S.map snd cs

parse :: String -> (Coords, [Fold])
parse = parseWith p
  where
    p :: Parser (Coords, [Fold])
    p = do
      (,) <$> (S.fromList <$> many point) <*> many folds
    point :: Parser Coord
    point = do
      x <- integer
      symbol ","
      y <- integer
      return (x, y)
    folds :: Parser Fold
    folds = do
      symbol "fold along"
      d <- symbol "x" <|> symbol "y"
      symbol "="
      dir d <$> integer
    dir "x" a = Vertical a
    dir "y" a = Horizontal a
    dir _ _ = error "parsing error"

solve :: String -> IO ()
solve input = putStrLn "--- Day 13 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
