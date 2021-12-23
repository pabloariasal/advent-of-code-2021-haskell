module Mat (Mat (..), Coord, neighboringCells, value, fromString, from2DList) where

import Data.Maybe (mapMaybe)
import qualified Data.Vector as V

type Coord = (Int, Int)

data Mat a = Mat {values :: V.Vector a, width :: Int, height :: Int} deriving (Eq, Show)

neighboringCells :: Mat a -> Coord -> [Coord]
neighboringCells m (r, c) = mapMaybe coord n
  where
    n = [(r + 1, c), (r - 1, c), (r, c - 1), (r, c + 1)]
    coord x = if inside x then Just x else Nothing
    inside (nr, nc) = nr >= 0 && nr < h && nc >= 0 && nc < w
    w = width m
    h = height m

value :: Mat a -> Coord -> a
value m (r, c) = values m V.! i
  where
    i = r * width m + c

fromString :: (Char -> a) -> String -> Mat a
fromString p s = Mat d width height
  where
    d = V.fromList $ concatMap parseLine (lines s)
    height = V.length d `div` width
    width = length (head $ lines s)
    parseLine = map p

from2DList :: [[a]] -> Mat a
from2DList lists = Mat (V.fromList $ concat lists) width height
  where
    width = length . head $ lists
    height = length lists
