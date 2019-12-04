-- Advent of Code 2019 Day 4 Part 1
-- https://adventofcode.com/2019/day/4#part2

passwords :: Int -> Int -> [Int]
passwords lower upper =
  [pass
  | pass <- [lower .. upper]
  , twoAdjacentRule pass
  , nonstrictIncrease pass]

twoAdjacentRule :: Int -> Bool
twoAdjacentRule n =           a == b && b != c
                 || a != b && b == c && c != d
                 || b != c && c == d && d != e
                 || c != d && d == e && e != f
                 || d != e && e == f
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

(!=) = (/=)
