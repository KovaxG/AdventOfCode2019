-- Advent of Code 2019 Day 1 Part 2
-- https://adventofcode.com/2019/day/1#part2

totalFuelRequired :: Int -> Int
totalFuelRequired mass
  | fuel > 0 = fuel + totalFuelRequired fuel
  | otherwise = 0
  where fuel = fuelRequired mass

fuelRequired :: Int -> Int
fuelRequired mass = div mass 3 - 2

main :: IO ()
main = do
  masses <- fmap read . lines <$> readFile "d01_1.in"
  let total = sum $ map totalFuelRequired masses
  putStrLn $ show total
