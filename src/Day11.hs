module Day11 (solve, part1, part2) where

import Control.Monad.State
import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)

type Coord = (Int, Int)

data Entry = Flashed | Energy Int deriving (Show, Eq)

type Mat = Map Coord Entry

part1 :: String -> String
part1 = show . sum . evalState (replicateM 100 step) . parse

-- todo fix this. This is kinda cheating I was very tired.
-- Ideally we would something like untilM
part2 :: String -> String
part2 = show . (+ 1) . fromJust . elemIndex 100 . evalState (replicateM 700 step) . parse

-- Increases the values of all octopi
-- flashes
-- resets
step :: State Mat Int
step = do
  modify (M.map increment)
  n <- flash
  modify (M.map unflash)
  return n

unflash :: Entry -> Entry
unflash Flashed = Energy 0
unflash (Energy x) = Energy x

increment :: Entry -> Entry
increment (Energy x) = Energy $ x + 1
increment Flashed = Flashed

-- 1) get all coords that must flash (bigger than 9 and unflashed)
-- 2) flash them
-- 3) increase value of neighbors
-- 4) go to 1
flash :: State Mat Int
flash = do
  m <- get
  let coordsToFlash = mustFlash m
  if null coordsToFlash
    then return 0
    else do
      modify (flashCoords coordsToFlash)
      let n = concatMap (`neighbors` m) coordsToFlash
      modify (incrementCoords n)
      i <- flash
      return (length coordsToFlash + i)

flashCoords :: [Coord] -> Mat -> Mat
flashCoords cs m = apply cs m (const Flashed)

incrementCoords :: [Coord] -> Mat -> Mat
incrementCoords cs m = apply cs m increment

apply :: [Coord] -> Mat -> (Entry -> Entry) -> Mat
apply [] m _ = m
apply (x : xs) m p = apply xs (M.adjust p x m) p

neighbors :: Coord -> Mat -> [Coord]
neighbors (r, c) m = filter (`M.member` m) n
  where
    n = [(r + dr, c + dc) | dr <- [-1 .. 1], dc <- [-1 .. 1], dr /= 0 || dc /= 0]

mustFlash :: Mat -> [Coord]
mustFlash = M.keys . M.filter f
  where
    f (Energy x) = x > 9
    f Flashed = False

parse :: String -> Mat
parse = M.fromList . zip coords . concatMap (map (Energy . digitToInt)) . lines
  where
    coords = [(r, c) | r <- [0 .. 9], c <- [0 .. 9]]

solve :: String -> IO ()
solve input = putStrLn "--- Day 11 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
