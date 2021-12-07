module Day02 (solve) where

import Control.Monad.State

part1 :: [(String, Int)] -> String
part1 = show . uncurry (*) . foldl f (0, 0)
  where
    f (p, dep) (dir, v) = case dir of
      "forward" -> (p + v, dep)
      "down" -> (p, dep + v)
      "up" -> (p, dep - v)
      _ -> error "Malformed input"

-- we could solve part2 similar to part1, but just for fun let's use the state monad instead 😁
part2 :: [(String, Int)] -> String
part2 s = show $ evalState (move s) (0, 0, 0)

move :: [(String, Int)] -> State (Int, Int, Int) Int
move [] = do
  (p, d, _) <- get
  return (p * d)
move (x : xs) = do
  s <- get
  put $ step x s
  move xs
  where
    step ("forward", s) (p, d, a) = (p + s, d + s * a, a)
    step ("up", s) (p, d, a) = (p, d, a - s)
    step ("down", s) (p, d, a) = (p, d, a + s)

parse :: [String] -> [(String, Int)]
parse = fmap $ s . words
  where
    s :: [String] -> (String, Int)
    s (x : y : _) = (x, read y)
    s _ = error "Malformed input"

solve :: String -> IO ()
solve input = putStrLn "--- Day 02 ---" >> putStrLn (part1 . parse . lines $ input) >> putStrLn (part2 . parse . lines $ input)
