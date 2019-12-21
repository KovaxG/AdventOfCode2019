{-# LANGUAGE BangPatterns #-}
-- Advent of Code 2019 Day 16 Part 2
-- https://adventofcode.com/2019/day/16#part2
import Data.Char
import System.CPUTime
import Debug.Trace

type Digits = String

doPhase :: [Int] -> [Int]
doPhase !n = map (calcDigit n) [1 .. calcNr] ++ rest
  where
    calcNr = div (length n) 2
    rest = map (flip mod 10) $ reverse $ tail $ scanl (+) 0 $ reverse $ drop calcNr n

calcDigit :: [Int] -> Int -> Int
calcDigit !ns !digitNr =
  flip mod 10 $ abs $ sum $ map (flip f digitNr) $ chunksOf (4 * digitNr) (23:ns)
  where
    f :: [Int] -> Int -> Int
    f !ns !digitNr = sum keep - sum neg
      where
        (_, rest1) = splitAt digitNr ns
        (keep,rest2) = splitAt digitNr rest1
        (_, rest3) = splitAt digitNr rest2
        (neg, _) = splitAt digitNr rest3


myProd :: Int -> Int -> Int
myProd 0 _ = 0
myProd 1 a = a
myProd (-1) a = -a

nplicate :: Int -> [Int] -> [Int]
nplicate !n !is = is >>= replicate n

toList :: Digits -> [Int]
toList !n = map (\c -> ord c - 48) n

main :: IO ()
main = do
  !input <- filter isDigit <$> readFile "d16_1.in"
  let !wtfInput = concat $ replicate 10000 input
  putStrLn $ "OrigLength: " ++ show (length input)
  putStrLn $ "NewLength:  " ++ show (length wtfInput)

  t0 <- getCPUTime
  putStrLn $ concat $ map show $ take 8 $ last $ take 101 $ iterate doPhase (toList input)
  t1 <- getCPUTime
  putStrLn $ show (div (t1 - t0) 1000000000) ++ " ms"

  t2 <- getCPUTime
  putStrLn $ show $ head $ concat $ map show $ doPhase (toList wtfInput)
  t3 <- getCPUTime
  putStrLn $ show (div (t3 - t2) 1000000000) ++ " ms"

chunksOf :: Int -> [a] -> [[a]]
chunksOf n as
  | length as > n = take n as : chunksOf n (drop n as)
  | otherwise = [as]
