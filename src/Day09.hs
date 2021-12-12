module Day09 (solve, part1, part2) where

import qualified Data.Vector as V
import Data.Maybe (mapMaybe)
import Data.Char (digitToInt)

type Coord = (Int, Int)
data Mat = Mat {values :: (V.Vector Int), width :: Int, height :: Int} deriving(Eq, Show)

part1 :: String -> String
part1 s = show $ sum (map get minima)
  where
        get c = let Just x = getVal m c in x + 1
        minima = filter (isLocalMin m) allCoords
        allCoords = [(r,c) | r <- [0..(h-1)], c <- [0..(w-1)]]
        h = height m
        w = width m
        m = parse s

part2 :: String -> String
part2 _ = "Not implemented"

parse :: String -> Mat
parse s = Mat d width height
  where
    d = V.fromList $ concat (map parseLine (lines s))
    height = (V.length d `div` width)
    width = (length (head $ lines s))
    parseLine = map digitToInt

isLocalMin :: Mat -> Coord -> Bool
isLocalMin m c = all (>e) (neighboringVals c m)
  where e = case getVal m c of
                 Just x -> x
                 Nothing -> error "element not in mat"

neighboringVals :: Coord -> Mat -> [Int]
neighboringVals (r, c) m = mapMaybe (getVal m) n
  where n = [(r + 1, c), (r - 1, c), (r, c - 1), (r, c + 1)]

getVal :: Mat -> Coord -> Maybe Int
getVal m c = getIndex c m >>= (\i -> (values m) V.!? i)

getIndex :: Coord -> Mat -> Maybe Int
getIndex (r, c) m = if inside then Just (r * w + c) else Nothing
                    where
                      inside = r >= 0 && r < h && c >= 0 && c < w
                      w = width m
                      h = height m

solve :: String -> IO ()
solve input = putStrLn "--- Day 09 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
