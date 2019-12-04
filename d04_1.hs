-- Advent of Code 2019 Day 4 Part 1
-- https://adventofcode.com/2019/day/4

passwords :: Int -> Int -> [Int]
passwords lower upper =
  [pass
  | pass <- [lower .. upper]
  , twoAdjacentSame pass
  , nonstrictIncrease pass]

twoAdjacentSame :: Int -> Bool
twoAdjacentSame n = a == b || b == c || c == d || d == e || e == f
  where
    [a, b, c, d, e, f] = toDigits n

nonstrictIncrease :: Int -> Bool
nonstrictIncrease n = a <= b && b <= c && c <= d && d <= e && e <= f
  where
    [a, b, c, d, e, f] = toDigits n

toDigits :: Int -> [Int]
toDigits n
  | n > 9 = toDigits (div n 10) ++ [mod n 10]
  | otherwise = [n]

main :: IO ()
main = do
  putStrLn $ show $ length $ passwords 256310 732736
