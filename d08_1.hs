-- Advent of Code 2019 Day 8 Part 1
-- https://adventofcode.com/2019/day/8
import Data.Char
import Data.List

width = 25
height = 6

chunksOf :: Int -> [a] -> [[a]]
chunksOf n as
  | length as > n = take n as : chunksOf n (drop n as)
  | otherwise = [as]

count :: Eq a => a -> [a] -> Int
count a = length . filter (==a)

zipMap :: (a -> b) -> [a] -> [(a,b)]
zipMap f = map (\a -> (a, f a))

check :: [Char] -> Int
check cs = ones * twos
  where
    ones = count '1' cs
    twos = count '2' cs

main :: IO ()
main = do
  contents <- readFile "d08_1.in"
  putStrLn $ show
           $ check
           $ fst
           $ minimumBy (\a b -> snd a `compare` snd b)
           $ zipMap (count '0')
           $ chunksOf (width * height)
           $ filter isDigit contents
