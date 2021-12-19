module Day14 (solve, part1, part2) where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Parsing
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char (letterChar, space)

type Rules = Map (Char, Char) Char

type Polymer = Map (Char, Char) Int

part1 :: String -> String
part1 = run 10

part2 :: String -> String
part2 = run 40

run :: Int -> String -> String
run it s =
  let (input, rules) = parse s
      cs = counts $ iterate (step rules) input !! it
      mostCommon = maximum cs
      leastCommon = minimum cs
   in show $ mostCommon - leastCommon

counts :: Polymer -> [Int]
counts = M.elems . M.map (\x -> (x + 1) `div` 2) . M.fromListWith (+) . concatMap f . M.toList
  where
    f ((a, b), c) = [(a, c), (b, c)]

step :: Rules -> Polymer -> Polymer
step rs = M.fromListWith (+) . foldr f [] . M.toList
  where
    f e@(p@(a, b), c) acc = case M.lookup p rs of
      Just x -> [((a, x), c), ((x, b), c)] ++ acc
      Nothing -> e : acc

parse :: String -> (Polymer, Rules)
parse = parseWith parser
  where
    parser :: Parser (Polymer, Rules)
    parser = (,) <$> fmap toPolymer letters <*> fmap M.fromList (many pair)
    pair :: Parser ((Char, Char), Char)
    pair = do
      [a, b] <- letters
      symbol "->"
      r <- letterChar
      space
      return ((a, b), r)

toPolymer :: String -> Polymer
toPolymer s = M.fromListWith (+) $ zip (zip s (tail s)) (repeat 1)

solve :: String -> IO ()
solve input = putStrLn "--- Day 14 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
