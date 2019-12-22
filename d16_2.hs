{-# LANGUAGE BangPatterns #-}
-- Advent of Code 2019 Day 16 Part 2
-- https://adventofcode.com/2019/day/16#part2
import Data.Char
import Data.List
import Debug.Trace

toList :: String -> [Int]
toList !n = map (\c -> ord c - 48) n

myPhase :: [Int] -> [Int]
myPhase !n = trace "Done" $ reverse
          $ map (flip mod 10)
          $ tail
          $ scanl (+) 0
          $ reverse n

main :: IO ()
main = do
  !input <- filter isDigit <$> readFile "d16_1.in"
  let !wtfInput = concat $ replicate 10000 input
  let !offset = read $ take 7 input :: Int
  let !myInput = drop offset wtfInput
  putStrLn $ show
           $ take 8
           $ last
           $ take 101
           $ iterate myPhase
           $ toList myInput
