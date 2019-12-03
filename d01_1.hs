-- Advent of Code 2019 Day 1 Part 1
-- https://adventofcode.com/2019/day/1

fuelRequired :: Int -> Int
fuelRequired mass = div mass 3 - 2

main :: IO ()
main = do
  masses <- fmap read . lines <$> readFile "d01_1.in"
  let totalFuelRequired = sum $ map fuelRequired masses
  putStrLn $ show totalFuelRequired
