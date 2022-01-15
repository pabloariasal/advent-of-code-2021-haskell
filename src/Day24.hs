module Day24 (solve, part1, part2) where

import Data.SBV hiding (solve)
import Parsing
import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Char (char, eol, space)

type Digit = SInt64

type Program = [Instruction]

data Reg = X | Y | Z | W deriving (Show)

data Operation = Add Reg (Either Reg Int) | Mul Reg (Either Reg Int) | Div Reg (Either Reg Int) | Mod Reg (Either Reg Int) | Eql Reg (Either Reg Int) deriving (Show)

data Instruction = Inp Reg | Op Operation deriving (Show)

data State = State {x :: Digit, y :: Digit, z :: Digit, w :: Digit} deriving (Eq, Show)

part1 :: String -> IO String
part1 s = show <$> maxModelNumber (parse s)

part2 :: String -> IO String
part2 s = show <$> minModelNumber (parse s)

parse :: String -> Program
parse = parseWith $ many (instruction <* space)
  where
    instruction :: Parser Instruction
    instruction = inp <|> binOp
    inp :: Parser Instruction
    inp = symbol "inp" >> Inp <$> reg
    binOp :: Parser Instruction
    binOp = do
      op <- letters
      from <- reg
      space
      to <- operand
      case op of
        "mul" -> return $ Op (Mul from to)
        "add" -> return $ Op (Add from to)
        "mod" -> return $ Op (Mod from to)
        "div" -> return $ Op (Div from to)
        "eql" -> return $ Op (Eql from to)
        _ -> error "unknown instruction"
    operand :: Parser (Either Reg Int)
    operand = Left <$> reg <|> Right <$> signedInteger
    reg :: Parser Reg
    reg = X <$ char 'x' <|> Y <$ char 'y' <|> Z <$ char 'z' <|> W <$ char 'w'

toNum :: [Digit] -> SInt64
toNum = foldl1 (\acc e -> acc * 10 + e)

applyOp :: State -> (Digit -> Digit -> Digit) -> Reg -> Either Reg Int -> State
applyOp s op to from = writeReg s to val
  where
    val = case from of
      Left reg -> op (readReg s to) (readReg s reg)
      Right v -> op (readReg s to) (fromIntegral v)

executeOperation :: State -> Operation -> State
executeOperation s (Add to from) = applyOp s (+) to from
executeOperation s (Mul to from) = applyOp s (*) to from
executeOperation s (Div to from) = applyOp s sDiv to from
executeOperation s (Mod to from) = applyOp s sMod to from
executeOperation s (Eql to from) = applyOp s (\a b -> ite (a .== b) 1 0) to from

writeReg :: State -> Reg -> Digit -> State
writeReg s r v = case r of
  X -> s {x = v}
  Y -> s {y = v}
  Z -> s {z = v}
  W -> s {w = v}

readReg :: State -> Reg -> Digit
readReg s r = case r of
  X -> x s
  Y -> y s
  Z -> z s
  W -> w s

run :: Program -> [Digit] -> State
run = go (State 0 0 0 0)
  where
    go :: State -> Program -> [Digit] -> State
    go state [] _ = state
    go state (Inp r : insts) (n : ns) = go (writeReg state r n) insts ns
    go state (Op op : insts) numbers = go (executeOperation state op) insts numbers
    go _ _ _ = undefined

isModelNumberValid :: Program -> [Digit] -> SBool
isModelNumberValid prog modelNum = sAll (\n -> n .>= 1 .&& n .<= 9) modelNum .&& z (run prog modelNum) .== 0

maxModelNumber :: Program -> IO OptimizeResult
maxModelNumber prog = optimize Lexicographic $ do
  numbers <- mkExistVars 14
  constrain $ isModelNumberValid prog numbers
  maximize "test" $ toNum numbers

minModelNumber :: Program -> IO OptimizeResult
minModelNumber prog = optimize Lexicographic $ do
  numbers <- mkExistVars 14
  constrain $ isModelNumberValid prog numbers
  minimize "test" $ toNum numbers

solve :: String -> IO ()
solve input = putStrLn "--- Day 24 ---" >> part1 input >>= putStrLn >> part2 input >>= putStrLn
