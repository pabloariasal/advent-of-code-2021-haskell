module Main where

import Control.Monad
import Data.Foldable
import qualified Data.Map as Map
import qualified Day01 (solve)
import qualified Day02 (solve)
import qualified Day03 (solve)
import qualified Day04 (solve)
import qualified Day05 (solve)
import qualified Day06 (solve)
import System.Environment
import System.Exit

solutions = Map.fromList [("01", Day01.solve),
                          ("02", Day02.solve),
                          ("03", Day03.solve),
                          ("04", Day04.solve),
                          ("05", Day05.solve),
                          ("06", Day06.solve)
                         ]

solveSingle :: String -> IO ()
solveSingle s = case Map.lookup s solutions of
  Just f -> readFile (concat ["./data/day", s, ".txt"]) >>= f
  Nothing -> putStrLn $ "Day not implemented: " ++ s

solveProblems :: [String] -> IO ()
solveProblems = mapM_ solveSingle

main :: IO ()
main = do
  args <- getArgs
  if null args
    then solveProblems $ Map.keys solutions
    else solveProblems args
