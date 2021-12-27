module Day19 (solve, part1, part2) where

import Data.Set (Set)
import qualified Data.Set as S
import Parsing
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char (char, eol, space, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Point = (Int, Int, Int)

type Scanner = Set Point

-- Transforms always convert a point in the candidate scanner's frame into the reference scanner's frae
type Transform = Point -> Point

pointAdd :: Point -> Point -> Point
pointAdd (p1, p2, p3) (q1, q2, q3) = (p1 + q1, p2 + q2, p3 + q3)

pointSub :: Point -> Point -> Point
pointSub (p1, p2, p3) (q1, q2, q3) = (p1 - q1, p2 - q2, p3 - q3)

-- https://stackoverflow.com/questions/16452383/how-to-get-all-24-rotations-of-a-3-dimensional-array
rotations :: [Transform]
rotations =
  [ id,
    rx,
    ry,
    rz,
    rx . rx,
    rx . ry,
    rx . rz,
    ry . rx,
    ry . ry,
    rz . ry,
    rz . rz,
    rx . rx . rx,
    rx . rx . ry,
    rx . rx . rz,
    rx . ry . rx,
    rx . ry . ry,
    rx . rz . rz,
    ry . rx . rx,
    ry . ry . ry,
    rz . rz . rz,
    rx . rx . rx . ry,
    rx . rx . ry . rx,
    rx . ry . rx . rx,
    rx . ry . ry . ry
  ]
  where
    rx (x, y, z) = (x, -z, y)
    ry (x, y, z) = (z, y, -x)
    rz (x, y, z) = (y, x, z)

transformations :: Scanner -> Scanner -> [Transform]
transformations s1 s2 = do
  p1 <- S.toList s1
  p2 <- S.toList s2
  rotation <- rotations
  return (translation rotation p1 p2 . rotation)
  where
    translation r p1 p2 x = x `pointAdd` (p1 `pointSub` (r p2))

overlap :: Scanner -> Scanner -> Maybe Transform
overlap s1 s2 = go (transformations s1 s2)
  where
    go :: [Transform] -> Maybe Transform
    go (t : ts)
      | (S.size $ S.intersection s1 (S.map t s2)) >= 12 = Just t
      | otherwise = go ts
    go [] = Nothing

extractNextOverlappingScanner :: Scanner -> [Scanner] -> (Scanner, [Scanner])
extractNextOverlappingScanner s sl = go s sl []
  where
    go :: Scanner -> [Scanner] -> [Scanner] -> (Scanner, [Scanner])
    go s (x : xs) discarded = case overlap s x of
      Nothing -> go s xs (x : discarded)
      Just t -> (S.map t x, discarded ++ xs)
    go _ [] _ = error "no overlapping scanner found"

run :: [Scanner] -> Scanner
run (s : sc) = go s sc
  where
    go :: Scanner -> [Scanner] -> Scanner
    go acc [] = acc
    go acc remaining =
      let (overlapping, rest) = extractNextOverlappingScanner acc remaining
       in go (S.union acc overlapping) rest

parse :: String -> [Scanner]
parse = parseWith (many scanner)
  where
    scanner :: Parser Scanner
    scanner = S.fromList <$> (header *> manyTill line blankLine <* space)
    line :: Parser Point
    line = (,,) <$> (si <* char ',') <*> (si <* char ',') <*> si <* eol
    header :: Parser Int
    header = symbol "--- scanner" *> integer <* string "---" <* eol
    blankLine = (() <$ eol) <|> eof
    si = L.signed space L.decimal

part1 :: String -> String
part1 = show . S.size . run . take 3 . parse

part2 :: String -> String
part2 _ = "Not implemented"

solve :: String -> IO ()
solve input = putStrLn "--- Day 19 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
