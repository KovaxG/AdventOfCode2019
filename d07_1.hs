-- Advent of Code 2019 Day 7 Part 1
-- https://adventofcode.com/2019/day/7

import qualified Data.Map as Map
import Data.Maybe
import Data.List

type Input = Int

data Computer = Computer {
  intcode :: Map.Map Int Int,
  programCounter :: Int,
  inputs :: [Int],
  outputs :: [Int]
} deriving (Show)

readInstruction :: Computer -> Int
readInstruction comp =
  readMemory (programCounter comp) comp

readMemory :: Int -> Computer -> Int
readMemory addr comp =
  fromJust $ Map.lookup addr (intcode comp)

writeMemory :: Int -> Int -> Computer -> Computer
writeMemory addr val comp =
  comp { intcode = Map.insert addr val $ intcode comp }

isTerminal :: Computer -> Bool
isTerminal computer =
  readInstruction computer == 99

binaryOp :: Int -> (Int -> Int -> Int) -> Computer -> Computer
binaryOp modes f comp =
  let pc = programCounter comp
      addr1 = readMemory (pc + 1) comp
      addr2 = readMemory (pc + 2) comp
      addr3 = readMemory (pc + 3) comp
      a = if modeParam1 == 0 then readMemory addr1 comp else addr1
      b = if modeParam2 == 0 then readMemory addr2 comp else addr2
      comp' = writeMemory addr3 (f a b) comp
  in comp' { programCounter = pc + 4 }
  where
    modeParam1 = mod modes 10
    modeParam2 = div (mod modes 100) 10


readInput :: Computer -> Computer
readInput comp =
  let pc = programCounter comp
      addr = readMemory (pc + 1) comp
      (input:rest) = inputs comp -- UNSAFE
      comp' = writeMemory addr input comp
  in comp' { inputs = rest, programCounter = pc + 2 }

writeOutput :: Int -> Computer -> Computer
writeOutput modes comp =
  let pc = programCounter comp
      addr = readMemory (pc + 1) comp
      value = if modeParam == 0 then readMemory addr comp else addr
  in comp { outputs = outputs comp ++ [value], programCounter = pc + 2 }
  where
    modeParam = mod modes 10


jumpOp :: Int -> (Int -> Bool) -> Computer -> Computer
jumpOp modes p comp =
  let pc = programCounter comp
      addr1 = readMemory (pc + 1) comp
      addr2 = readMemory (pc + 2) comp
      cond = if modeParam1 == 0 then readMemory addr1 comp else addr1
      jumpTo = if modeParam2 == 0 then readMemory addr2 comp else addr2
      pc' = if p cond then jumpTo else pc + 3
  in comp { programCounter =  pc' }
  where
    modeParam1 = mod modes 10
    modeParam2 = div (mod modes 100) 10

tick :: Computer -> Computer
tick comp =
  let instruction = readInstruction comp
      (modes, opcode) = divMod instruction 100
  in case opcode of
      99 -> comp                         -- HALT
      1 -> binaryOp modes (+) comp       -- ADD
      2 -> binaryOp modes (*) comp       -- MUL
      3 -> readInput comp                -- READ
      4 -> writeOutput modes comp        -- WRITE
      5 -> jumpOp modes (!=0) comp       -- JUMPIFTRUE
      6 -> jumpOp modes (==0) comp       -- JUMPIFFALSE
      7 -> binaryOp modes (\a b -> if a < b then 1 else 0) comp -- LESSTHAN
      8 -> binaryOp modes (\a b -> if a == b then 1 else 0) comp -- EQUALS
      somethingElse -> error $ "Unrecognized opcode! (" ++ show somethingElse ++ ")"

parseInput :: String -> Computer
parseInput str =
  Computer {
    programCounter = 0,
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

runUntilHalt :: Computer -> Computer
runUntilHalt comp
  | isTerminal comp = comp
  | otherwise = runUntilHalt (tick comp)

runFor :: Int -> Computer -> Int
runFor input comp = head
                  $ outputs
                  $ runUntilHalt
                  $ comp { inputs = inputs comp ++ [input] }

runAmps :: Int -> [Computer] -> Int
runAmps input [c] = runFor input c
runAmps input (c:cs) = runAmps (runFor input c) cs

main :: IO ()
main = do
  comp <- parseInput <$> readFile "d07_1.in"
  putStrLn $ show
           $ maximum
           $ map (runAmps 0)
           $ map (map (\v -> comp {inputs=[v]}))
           $ permutations [0 .. 4]

(!=) = (/=)
