module Day03 (solve) where

import Data.Char (digitToInt)
import Data.List (transpose)

data Bit = One | Zero deriving (Eq, Show)
 
part1 :: String -> String
part1 s = show $ toInt mostOccurringBitPerCol * toInt (map invert mostOccurringBitPerCol)
  where
    mostOccurringBitPerCol = map mostOcurringBit (transpose . toBitMatrix $ s)

mostOcurringBit :: [Bit] -> Bit
mostOcurringBit l = if numOnes >= numZeros then One else Zero
  where
    numOnes = length $ filter (== One) l
    numZeros = length $ filter (== Zero) l

toBitMatrix :: String -> [[Bit]]
toBitMatrix = map toBitList . lines

toBitList :: String -> [Bit]
toBitList = map (toBit . digitToInt)

invert :: Bit -> Bit
invert Zero = One
invert _ = Zero

toBit :: Int -> Bit
toBit 0 = Zero
toBit 1 = One
toBit _ = error "Bad input"

-- takes a binary number like [0, 1, 0] and converts it to an Int
toInt :: [Bit] -> Int
toInt l = foldl (\acc bit -> acc * 2 + bit) 0 ints
  where
    ints = map parseInt l
    parseInt One = 1
    parseInt Zero = 0

---- Part 2
part2 :: String -> String
part2 s = let
  bits = toBitMatrix s
  oxygenRating = filterUntil mostOcurringBit 0 bits
  co2rating = filterUntil (invert . mostOcurringBit) 0 bits
  in show $ toInt oxygenRating * toInt co2rating

-- Filters the matrix until
filterUntil:: ([Bit] -> Bit) -> Int -> [[Bit]] -> [Bit]
filterUntil _ _ [x] = x
filterUntil f i l = filterUntil f (i + 1) $ filter ((==target) . (!!i)) l
  where
    target = f col
    col = map (!!i) l

solve :: String -> IO ()
solve input = putStrLn "--- Day 03 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
