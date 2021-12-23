module Day16 (solve, part1, part2) where

import Data.Char (digitToInt, intToDigit)
import Data.Foldable (foldl')
import qualified Data.Text as T
import Parsing
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Read (Lexeme (String))

data Packet = Packet {version :: Int, typeId :: Int, payload :: Payload} deriving (Eq, Show)

data Payload = Literal Int | Operator [Packet] deriving (Eq, Show)

part1 :: String -> String
part1 s = show . go $ parseWith parsePackets (hexToBinary s)
  where
    go p = case payload p of
      Literal _ -> version p
      Operator ps -> version p + sum (map go ps)

part2 :: String -> String
part2 s = show . eval $ parseWith parsePackets (hexToBinary s)

eval :: Packet -> Int
eval (Packet _ _ (Literal n)) = n
eval (Packet _ t (Operator ps)) = case t of
  0 -> sum $ map eval ps
  1 -> product $ map eval ps
  2 -> minimum $ map eval ps
  3 -> maximum $ map eval ps
  5 -> if first ps > second ps then 1 else 0
  6 -> if first ps < second ps then 1 else 0
  7 -> if first ps == second ps then 1 else 0
  _ -> error "operation not permitted"
  where
    first ps = eval $ head ps
    second ps = eval $ last ps

parsePackets :: Parser Packet
parsePackets = parsePacket <* (many (char '0') *> eof)

parsePacket :: Parser Packet
parsePacket = do
  (v, t) <- parseHeader
  if t == 4
    then Packet v t <$> parseLiteralValuePayload
    else Packet v t <$> parseOperatorPayload

parseLiteralValuePayload :: Parser Payload
parseLiteralValuePayload = Literal . readNum <$> ((++) <$> firstPart <*> zeroGroup)
  where
    firstPart :: Parser String
    firstPart = concat <$> many oneGroup
    oneGroup :: Parser String
    oneGroup = char '1' >> fourBits
    zeroGroup :: Parser String
    zeroGroup = char '0' >> fourBits
    fourBits :: Parser [Char]
    fourBits = count 4 parseBit

parseOperatorPayload :: Parser Payload
parseOperatorPayload = try parseBitCountPayload <|> parsePacketCountPayload

parseBitCountPayload :: Parser Payload
parseBitCountPayload = do
  char '0'
  n <- parseNum 15
  Operator <$> go n
  where
    go :: Int -> Parser [Packet]
    go 0 = return []
    go remainingBits = do
      before <- getParserState
      p <- parsePacket
      after <- getParserState
      remaining <- go (remainingBits - (stateOffset after - stateOffset before))
      return $ p : remaining

parsePacketCountPayload :: Parser Payload
parsePacketCountPayload = do
  char '1'
  n <- parseNum 11
  Operator <$> count n parsePacket

readNum :: String -> Int
readNum = foldl' step 0
  where
    step acc b = 2 * acc + digitToInt b

parseNum :: Int -> Parser Int
parseNum n = readNum <$> count n parseBit

parseBit :: Parser Char
parseBit = '0' <$ char '0' <|> '1' <$ char '1'

parseHeader :: Parser (Int, Int)
parseHeader = (,) <$> parseNum 3 <*> parseNum 3

hexToBinary :: String -> String
hexToBinary = map intToDigit . concatMap (fill . reverse . intToBin . digitToInt) . strip
  where
    intToBin :: Int -> [Int]
    intToBin 0 = []
    intToBin n = let (q, r) = n `divMod` 2 in r : intToBin q
    fill :: [Int] -> [Int]
    fill nums = go (4 - length nums) nums
      where
        go 0 l = l
        go n l = 0 : go (n - 1) l

strip :: String -> String
strip = T.unpack . T.strip . T.pack

solve :: String -> IO ()
solve input = putStrLn "--- Day 16 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
