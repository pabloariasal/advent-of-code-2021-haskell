module Day20 (solve, part1, part2) where

import Data.Array (Array)
import qualified Data.Array as A
import Data.Bits (testBit)
import Data.Foldable (foldl')
import Data.List.Split (splitOn)

-- given an index value, return the value to replace
type Algorithm = Int -> Bool

-- access is always (row, col)
type Image = Array (Int, Int) Bool

part1 :: String -> String
part1 = run 2

part2 :: String -> String
part2 = run 50

run :: Int -> String -> String
run n s =
  let (algoData, img) = parse s
      algo = testBit algoData
   in show . length . filter id . A.elems . snd $ iterate (enhance algo) (False, img) !! n

enhance :: Algorithm -> (Bool, Image) -> (Bool, Image)
enhance algo (f, img) = (algo . binToInt $ replicate 9 f, A.listArray (A.bounds padded) $ map (algo . binToInt . kernelVal (expand f padded A.!)) (A.range $ A.bounds padded))
  where
    padded = expand f img
    kernelVal :: ((Int, Int) -> Bool) -> (Int, Int) -> [Bool]
    kernelVal readVal (r, c) = [readVal (r + dx, c + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1]]

expand :: Bool -> Image -> Image
expand f img = A.accumArray (\_ x -> x) f ((minR - 1, minC - 1), (maxR + 1, maxC + 1)) (A.assocs img)
  where
    ((minR, minC), (maxR, maxC)) = A.bounds img

parse :: String -> (Integer, Image)
parse s =
  let [algo, image] = splitOn "\n\n" s
      values = lines image
      width = length (head values) - 1
      height = length values - 1
   in (binToInt . reverse . map (== '#') $ algo :: Integer, A.listArray ((0, 0), (width, height)) $ values >>= readRow)
  where
    readRow = map (== '#')

binToInt :: Num a => [Bool] -> a
binToInt = foldl' (\acc bit -> acc * 2 + if bit then 1 else 0) 0

solve :: String -> IO ()
solve input = putStrLn "--- Day 20 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
