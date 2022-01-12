module Day23 (solve, part1, part2) where

import Data.List (findIndex, transpose)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Dijkstra (findPath)
import Parsing
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char

type Rooms = Vector [Char]

type Hall = Map Int Char

type Burrow = (Rooms, Hall, Int)

cost :: Char -> Int
cost 'A' = 1
cost 'B' = 10
cost 'C' = 100
cost 'D' = 1000
cost _ = error "amphipod unknown"

end1 :: Burrow
end1 = (V.fromList ["AA", "BB", "CC", "DD"], M.empty, 2)

end2 :: Burrow
end2 = undefined

range :: Int -> Int -> [Int]
range x y = if x < y then [x .. y] else [y .. x]

roomPos :: Int -> Int
roomPos r = (r + 1) * 2

allHallwayPositions :: [Int]
allHallwayPositions = [0, 1, 3, 5, 7, 9, 10]

exitHallwayPositions :: Burrow -> Int -> [Int]
exitHallwayPositions b@(rooms, hall, _) r
  | null (rooms V.! r) = []
  | otherwise = [h | h <- allHallwayPositions, isFree h]
  where
    isFree h = all (`M.notMember` hall) (range h (roomPos r))

exitRoom :: Burrow -> Int -> Int -> (Burrow, Int)
exitRoom b@(rooms, hall, size) r h = ((newRooms, newHall, size), c)
  where
    newRooms = rooms V.// [(r, tail room)]
    newHall = M.insert h a hall
    c = cost a * ((size - length room) + 1 + abs (roomPos r - h))
    a = head room
    room = rooms V.! r

roomExits :: Burrow -> [(Burrow, Int)]
roomExits b = [exitRoom b r h | r <- [0 .. 3], h <- exitHallwayPositions b r]

enterRoom :: Burrow -> Int -> Int -> (Burrow, Int)
enterRoom b@(rooms, hall, size) r h = ((newRooms, newHall, size), c)
  where
    newRooms = rooms V.// [(r, a : room)]
    newHall = M.delete h hall
    c = cost a * ((size - length room) + abs (roomPos r - h))
    a = hall M.! h
    room = rooms V.! r

roomDestination :: Burrow -> Int -> [Int]
roomDestination b@(rooms, hall, size) h
  | doesNotContainOtherAmphipods && isFree && isNotFull = [roomIndex]
  | otherwise = []
  where
    doesNotContainOtherAmphipods = not $ any (/= a) room
    isNotFull = length room < size
    isFree = and [h' `M.notMember` hall | h' <- range (roomPos roomIndex) h, h' /= h]
    room = rooms V.! roomIndex
    roomIndex = dest a
    a = hall M.! h
    dest 'A' = 0
    dest 'B' = 1
    dest 'C' = 2
    dest 'D' = 3
    dest _ = error "amphipod unkown"

roomEnters :: Burrow -> [(Burrow, Int)]
roomEnters b@(rooms, hall, _) = [enterRoom b r h | h <- M.keys hall, r <- roomDestination b h]

step :: Burrow -> [(Burrow, Int)]
step b = roomExits b ++ roomEnters b

run :: Burrow -> Burrow -> Int
run start end = snd . last $ findPath start end step

parse :: String -> Burrow
parse s = parseWith b (input s)
  where
    b :: Parser Burrow
    b = do
      rs <- transpose <$> many row
      return (V.fromList rs, M.empty, length . head $ rs)
    row :: Parser [Char]
    row = do
      many sep
      as <- count 4 (amphipod <* sep)
      many sep
      return as
    amphipod :: Parser Char
    amphipod = choice [char 'A', char 'B', char 'C', char 'D']
    sep = symbol "#"
    input s = let t = drop 2 $ lines s in unlines t

part1 :: String -> String
part1 s = show $ run (parse s) end1

part2 :: String -> String
part2 _ = "Not implemented"

solve :: String -> IO ()
solve input = putStrLn "--- Day 23 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
