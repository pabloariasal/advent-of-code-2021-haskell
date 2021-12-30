{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Day21 (solve, part1, part2) where

import Control.Monad.State
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Tuple (swap)
import Debug.Trace
import Parsing
import Text.Megaparsec hiding (State, parse)

data Player = Player {score :: Int, position :: Int} deriving (Eq, Show, Ord)

type Game = State Int [Player]

part1 :: String -> String
part1 s =
  let (ps, dice) = runState (play . parse $ s) 0
   in show $ minimum (map score ps) * dice

play :: [Player] -> Game
play ps
  | any ((>= 1000) . score) ps = return ps
  | otherwise = gameStep ps >>= play

gameStep :: [Player] -> Game
gameStep (p : ps) = do
  n <- sum <$> replicateM 3 (modify (+ 1) *> get)
  return $ ps ++ [move n p]

move :: Int -> Player -> Player
move roll player = Player (score player + newPos) newPos
  where
    newPos = (position player + roll) `mod1` 10

mod1 :: Int -> Int -> Int
mod1 a b = ((a - 1) `mod` b) + 1

part2 :: String -> String
part2 s =
  let (p1 : p2 : _) = parse s
      (s1, s2) = evalState (solveGameDP (p1, p2)) M.empty
   in show $ max s1 s2

solveGameDP :: (Player, Player) -> State (M.Map (Player, Player) (Int, Int)) (Int, Int)
solveGameDP game@(p1, p2)
  | score p1 >= 21 = return (1, 0)
  | score p2 >= 21 = return (0, 1)
  | otherwise = do
      dp <- get
      case M.lookup game dp of
        Just r -> return r
        _ -> do
          res <- swap . foldl' addScores (0, 0) <$> mapM solveGameDP [(p2, move n p1) | n <- sum <$> replicateM 3 [1, 2, 3]]
          modify (M.insert game res)
          return res

addScores :: (Int, Int) -> (Int, Int) -> (Int, Int)
addScores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

parse :: String -> [Player]
parse = parseWith (many player)
  where
    player = do
      symbol "Player"
      _ <- integer
      symbol "starting position:"
      Player 0 <$> integer

solve :: String -> IO ()
solve input = putStrLn "--- Day 21 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
