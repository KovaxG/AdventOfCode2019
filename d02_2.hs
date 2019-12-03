-- Advent of Code 2019 Day 2 Part 2
-- https://adventofcode.com/2019/day/2#part2


import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe
import Debug.Trace

data Computer = Computer {
  intcode :: Map.Map Int Int,
  programCounter :: Int
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

binaryOp :: (Int -> Int -> Int) -> Computer -> Computer
binaryOp f comp =
  let pc = programCounter comp
      addr1 = readMemory (pc + 1) comp
      addr2 = readMemory (pc + 2) comp
      addr3 = readMemory (pc + 3) comp
      a = readMemory addr1 comp
      b = readMemory addr2 comp
      newComp = writeMemory addr3 (f a b) comp
  in newComp { programCounter = pc + 4 }

tick :: Computer -> Computer
tick comp = case readInstruction comp of
  99 -> comp
  1 -> binaryOp (+) comp
  2 -> binaryOp (*) comp
  somethingElse -> error $ "Unrecognized opcode! (" ++ show somethingElse ++ ")"

parseInput :: String -> Computer
parseInput str =
  Computer {
    programCounter = 0,
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

generateComputers :: Computer -> [((Int, Int), Computer)]
generateComputers comp =
  [ ((noun, verb), writeMemory 2 verb
  $ writeMemory 1 noun
  $ comp)
  | noun <- [0 .. 99]
  , verb <- [0 .. 99]]

main :: IO ()
main = do
  rawText <- readFile "d02_1.in"
  let answ = fmap (\((noun, verb), _) -> 100 * noun + verb)
           $ List.find (\(t, v) -> v == 19690720)
           $ map (\(t, c) -> (t, readMemory 0 c))
           $ map (\(t, c) -> (t, runUntilHalt c))
           $ generateComputers
           $ parseInput rawText
  putStrLn $ show answ
