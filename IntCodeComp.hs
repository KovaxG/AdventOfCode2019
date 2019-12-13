module IntCodeComp (
 Computer (..),
 isTerminal,
 tick,
 runUntil,
 runWithInputUntil,
 parseInput,
 writeMemory
) where

import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Debug.Trace

data Mode = Immediate | Position | Relative deriving (Show, Eq)

type Input = Int
type Address = Int
type Value = Int

data Computer = Computer {
  intcode :: Map.Map Int Int,
  programCounter :: Int,
  relativeBase :: Int,
  inputs :: [Int],
  outputs :: [Int]
} deriving (Show)

readInstruction :: Computer -> Int
readInstruction comp =
  readMemory (programCounter comp) comp

readMemory :: Int -> Computer -> Int
readMemory addr comp =
  fromMaybe 0 $ Map.lookup addr (intcode comp)

writeMemory :: Int -> Int -> Computer -> Computer
writeMemory addr val comp =
  comp { intcode = Map.insert addr val $ intcode comp }

isTerminal :: Computer -> Bool
isTerminal computer =
  readInstruction computer == 99

writeWithMode :: Mode -> Computer -> Address -> Value -> Computer
writeWithMode mode comp addr val =
  case mode of
    Position -> writeMemory addr val comp
    Relative -> writeMemory (addr + relativeBase comp) val comp

readWithMode :: Mode -> Computer -> Address -> Value
readWithMode mode comp addr =
  case mode of
    Immediate -> addr
    Position  -> readMemory addr comp
    Relative  -> readMemory (addr + relativeBase comp) comp

binaryOp :: Int -> (Int -> Int -> Int) -> Computer -> Computer
binaryOp modes f comp =
  let pc = programCounter comp
      addr1 = readMemory (pc + 1) comp
      addr2 = readMemory (pc + 2) comp
      addr3 = readMemory (pc + 3) comp
      a = readWithMode m1 comp addr1
      b = readWithMode m2 comp addr2
      comp' = writeWithMode m3 comp addr3 (f a b)
  in comp' { programCounter = pc + 4 }
  where
    [m1, m2, m3] = parseModes 3 modes


readInput :: Int -> Computer -> Computer
readInput modes comp =
  let pc = programCounter comp
      addr = readMemory (pc + 1) comp
      (input:rest) = inputs comp
      comp' = writeWithMode m comp addr input
  in comp' { inputs = rest, programCounter = pc + 2 }
  where
    [m] = parseModes 1 modes

jumpOp :: Int -> (Int -> Bool) -> Computer -> Computer
jumpOp modes p comp =
  let pc = programCounter comp
      addr1 = readMemory (pc + 1) comp
      addr2 = readMemory (pc + 2) comp
      cond   = readWithMode m1 comp addr1
      jumpTo = readWithMode m2 comp addr2
      pc' = if p cond then jumpTo else pc + 3
  in comp { programCounter =  pc' }
  where
    [m1, m2] = parseModes 2 modes

singleOp :: Int -> (Int -> Computer -> Computer) -> Computer -> Computer
singleOp modes f comp =
  let pc = programCounter comp
      addr = readMemory (pc + 1) comp
      value = readWithMode m comp addr
      comp' = f value comp
  in comp' { programCounter = pc + 2 }
  where
    [m] = parseModes 1 modes

tick :: Computer -> Computer
tick comp =
  let instruction = readInstruction comp
      (modes, opcode) = divMod instruction 100
  in case opcode of
      99 -> comp                         -- HALT
      1 -> binaryOp modes (+) comp       -- ADD
      2 -> binaryOp modes (*) comp       -- MUL
      3 -> readInput modes comp -- READ
      4 -> singleOp modes (\v c -> c { outputs = outputs c ++ [v] }) comp        -- WRITE
      5 -> jumpOp modes (!=0) comp       -- JUMPIFTRUE
      6 -> jumpOp modes (==0) comp       -- JUMPIFFALSE
      7 -> binaryOp modes (\a b -> if a < b then 1 else 0) comp -- LESSTHAN
      8 -> binaryOp modes (\a b -> if a == b then 1 else 0) comp -- EQUALS
      9 -> singleOp modes (\v c -> c { relativeBase = relativeBase c + v }) comp -- AddtoR
      somethingElse -> error $ "Unrecognized opcode! (" ++ show somethingElse ++ ")"

parseInput :: String -> Computer
parseInput str =
  Computer {
    programCounter = 0,
    relativeBase = 0,
    inputs = [],
    outputs = [],
    intcode =
      Map.fromList
      $ zip [0 ..]
      $ map read
      $ words
      $ map (\c -> if c == ',' then ' ' else c)
      $ str
  }

runUntil :: (Computer -> Bool) -> Computer -> Computer
runUntil cond comp
  | cond comp = comp
  | otherwise = runUntil cond $ tick comp

runWithInputUntil :: (Computer -> Bool) -> Int -> Computer -> Computer
runWithInputUntil cond input comp =
  runUntil cond $ comp {inputs=inputs comp ++ [input]}

parseModes :: Int -> Int -> [Mode]
parseModes n m
  | n == 1 = [toMode (mod m 10)]
  | otherwise = toMode (mod m 10) : parseModes (n-1) (div m 10)
  where
    toMode :: Int -> Mode
    toMode i = case i of
      0 -> Position
      1 -> Immediate
      2 -> Relative

(!=) = (/=)
