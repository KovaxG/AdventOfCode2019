{-# LANGUAGE BangPatterns #-}
-- Advent of Code 2019 Day 16 Part 1
-- https://adventofcode.com/2019/day/16
import Data.Char
import Debug.Trace

type Digits = String

doPhase :: [Int] -> [Int]
doPhase !n = map (calcDigit n) [1 .. length n]

calcDigit :: [Int] -> Int -> Int
calcDigit !ns !digitNr = flip mod 10 $ abs $ sum $ zipWith (*) ns pattern
  where pattern = drop 1 $ cycle $ nplicate digitNr [0,1,0,-1]

nplicate :: Int -> [Int] -> [Int]
nplicate !n !is = is >>= replicate n

toList :: Digits -> [Int]
toList !n = map (\c -> ord c - 48) n

main :: IO ()
main = do
  !input <- filter isDigit <$> readFile "d16_1.in"
  putStrLn $ concat $ map show $ take 8 $ last $ take 101 $ iterate doPhase (toList input)
