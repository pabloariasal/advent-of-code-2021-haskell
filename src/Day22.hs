module Day22 (solve, part1, part2) where

import Data.Foldable (foldl')
import Data.Maybe (isJust, mapMaybe)
import Parsing
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char (char)

type Span = (Int, Int)

data Cuboid = Cuboid {spanX :: Span, spanY :: Span, spanZ :: Span} deriving (Eq, Ord, Show)

data Action = On | Off deriving (Eq, Show)

data EngineState = EngineState {adds :: [Cuboid], subs :: [Cuboid]}

-- we must substract the cuboid's intersection with the adds
step :: (Action, Cuboid) -> EngineState -> EngineState
step (a, c) EngineState {..} = case a of
  On -> EngineState (c : cuboidsToAdd) cuboidsToSubstract
  _ -> EngineState cuboidsToAdd cuboidsToSubstract
  where
    cuboidsToAdd = intersectionWithSubs ++ adds
    intersectionWithSubs = mapMaybe (intersectCuboid c) subs
    cuboidsToSubstract = intersectionWithAdds ++ subs
    intersectionWithAdds = mapMaybe (intersectCuboid c) adds

spanCardinality :: Span -> Int
spanCardinality (a, b) = b - a + 1

intersectSpan :: Span -> Span -> Maybe Span
intersectSpan (a1, a2) (b1, b2)
  | max1 <= min2 = Just (max1, min2)
  | otherwise = Nothing
  where
    max1 = max a1 b1
    min2 = min a2 b2

intersectCuboid :: Cuboid -> Cuboid -> Maybe Cuboid
intersectCuboid (Cuboid x1 y1 z1) (Cuboid x2 y2 z2) = do
  xspan <- intersectSpan x1 x2
  yspan <- intersectSpan y1 y2
  zspan <- intersectSpan z1 z2
  return $ Cuboid xspan yspan zspan

cuboidCardinality :: Cuboid -> Int
cuboidCardinality Cuboid {..} = product $ map spanCardinality [spanX, spanY, spanZ]

engineCardinality :: EngineState -> Int
engineCardinality EngineState {..} = sum (map cuboidCardinality adds) - sum (map cuboidCardinality subs)

filterCubes :: [(Action, Cuboid)] -> [(Action, Cuboid)]
filterCubes = filter (isJust . intersectCuboid ref . snd)
  where
    ref = Cuboid (-50, 50) (-50, 50) (-50, 50)

initialEngine :: EngineState
initialEngine = EngineState [] []

part1 :: String -> String
part1 = show . engineCardinality . foldl' (flip step) initialEngine . filterCubes . parse

part2 :: String -> String
part2 = show . engineCardinality . foldl' (flip step) initialEngine . parse

parse :: String -> [(Action, Cuboid)]
parse = parseWith (many cuboidAction)
  where
    cuboidAction :: Parser (Action, Cuboid)
    cuboidAction = do
      a <- action
      xs <- span
      symbol ","
      ys <- span
      symbol ","
      zs <- span
      return (a, Cuboid xs ys zs)
    action = On <$ symbol "on" <|> Off <$ symbol "off"
    span = do
      c
      symbol "="
      b <- signedInteger
      symbol ".."
      e <- signedInteger
      return (b, e)
    c = char 'x' <|> char 'y' <|> char 'z'

solve :: String -> IO ()
solve input = putStrLn "--- Day 22 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
