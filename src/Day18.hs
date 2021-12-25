module Day18 (solve, part1, part2) where

import Control.Applicative
import Data.List (foldl1')
import Data.Maybe
import qualified Data.Text as T
import Parsing
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char (char, eol)

-- see https://wiki.haskell.org/Zipper
data Tree a = Leaf a | Fork (Tree a) (Tree a) deriving (Eq)

instance (Show a) => Show (Tree a) where
  show (Leaf n) = show n
  show (Fork l r) = "[" ++ show l ++ "," ++ show r ++ "]"

data Cxt a = Top | L (Cxt a) (Tree a) | R (Tree a) (Cxt a)

type SFNum = Tree Int

type Loc = (SFNum, Cxt Int)

-- returns the left subtree
left :: Loc -> Loc
left (Fork l r, c) = (l, L c r)
left _ = error "should never happen"

-- returns the right subtree
right :: Loc -> Loc
right (Fork l r, c) = (r, R l c)
right _ = error "should never happen"

top :: SFNum -> Loc
top t = (t, Top)

up :: Loc -> Loc
up (t, L c r) = (Fork t r, c)
up (t, R l c) = (Fork l t, c)
up _ = error "should never happen"

upmost :: Loc -> Loc
upmost l@(t, Top) = l
upmost l = upmost (up l)

modify :: Loc -> (SFNum -> SFNum) -> Loc
modify (t, c) f = (f t, c)

-- note, loc here must be in a left node
mostRightOnLeft :: Loc -> Maybe Loc
mostRightOnLeft (_, Top) = Nothing
mostRightOnLeft t@(_, L _ _) = mostRightOnLeft $ up t
mostRightOnLeft t@(_, R _ _) = Just $ rightMost $ left $ up t

rightMost :: Loc -> Loc
rightMost t@(Leaf _, _) = t
rightMost t@(Fork _ _, _) = rightMost $ right t

-- note, loc here must be in a right node
mostLeftOnRight :: Loc -> Maybe Loc
mostLeftOnRight (_, Top) = Nothing
mostLeftOnRight t@(_, L _ _) = Just $ leftMost $ right $ up t
mostLeftOnRight t@(_, R _ _) = mostLeftOnRight $ up t

leftMost :: Loc -> Loc
leftMost t@(Leaf _, _) = t
leftMost t@(Fork _ _, _) = leftMost $ left t

findSplittableSubtree :: SFNum -> Maybe Loc
findSplittableSubtree = go . top
  where
    go t@(Leaf n, _)
      | n >= 10 = Just t
      | otherwise = Nothing
    go t@(_, _) = go (left t) <|> go (right t)

extractNum :: Loc -> SFNum
extractNum = fst . upmost

split :: SFNum -> Maybe SFNum
split t = case findSplittableSubtree t of
  Nothing -> Nothing
  Just l -> Just $ extractNum . splitLoc $ l
  where
    splitLoc t@(Leaf n, _) = modify t (\_ -> Fork (Leaf $ ln n) (Leaf $ rn n))
    ln x = x `div` 2
    rn x = ln x + x `mod` 2

findExplodableSubtree :: SFNum -> Maybe Loc
findExplodableSubtree num = go 4 (top num)
  where
    go :: Int -> Loc -> Maybe Loc
    go _ t@(Leaf _, _) = Nothing
    go 0 t@(Fork _ _, _) = Just t
    go n t@(Fork _ _, _) = go (n - 1) (left t) <|> go (n - 1) (right t)

explode :: SFNum -> Maybe SFNum
explode t = case findExplodableSubtree t of
  Nothing -> Nothing
  Just n@(Fork (Leaf lv) (Leaf rv), _) -> Just $ setLocToZero . addLeft lv . addRight rv . extractNum $ n
  Just _ -> error "this should never happen"

setLocToZero :: SFNum -> SFNum
setLocToZero t =
  let locToSplit = fromJust $ findExplodableSubtree t
   in extractNum $ modify locToSplit (\_ -> Leaf 0)

addLeft :: Int -> SFNum -> SFNum
addLeft v t =
  let locToSplit = fromJust $ findExplodableSubtree t
   in addToLoc locToSplit v mostRightOnLeft

addRight :: Int -> SFNum -> SFNum
addRight v t =
  let locToSplit = fromJust $ findExplodableSubtree t
   in addToLoc locToSplit v mostLeftOnRight

addToLoc :: Loc -> Int -> (Loc -> Maybe Loc) -> SFNum
addToLoc l n f = case f l of
  Nothing -> extractNum l
  Just t@(Leaf v, _) -> extractNum $ modify t (\_ -> Leaf $ v + n)
  Just _ -> error "this should never happen"

reduce :: SFNum -> SFNum
reduce n = maybe n reduce (explode n <|> split n)

strip :: String -> String
strip = T.unpack . T.strip . T.pack

parse :: String -> [SFNum]
parse = parseWith (number `sepBy1` eol) . strip
  where
    number = leaf <|> combined
    leaf = Leaf <$> integer
    combined = Fork <$> (char '[' *> number) <*> (char ',' *> number) <* char ']'

addNums :: SFNum -> SFNum -> SFNum
addNums l r = reduce $ Fork l r

magnitude :: SFNum -> Int
magnitude (Leaf n) = n
magnitude (Fork l r) = 3 * magnitude l + 2 * magnitude r

part1 :: String -> String
part1 = show . magnitude . foldl1' addNums . parse

part2 :: String -> String
part2 = show . maximum . allSums . parse
  where
    allSums n = do
      a <- n
      magnitude . addNums a <$> n

solve :: String -> IO ()
solve input = putStrLn "--- Day 18 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
