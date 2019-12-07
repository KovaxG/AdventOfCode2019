-- Advent of Code 2019 Day 6 Part 1
-- https://adventofcode.com/2019/day/6
import Data.Tree

makeTree :: [(String, String)] -> Tree (Int, String)
makeTree dl =
  unfoldTree (\(l, s) -> ((l, s), map (\(_,v) -> (l+1,v)) $ filter ((==s) . fst) dl)) (0, "COM")

parseString :: String -> [(String, String)]
parseString = map ((\[a,b] -> (a,b)) . words . repSep) . lines
  where repSep = map (\c -> if c == ')' then ' ' else c)

main :: IO ()
main = do
  dl <- parseString <$> readFile "d06_1.in"
  putStrLn $ show $ sum $ map fst $ flatten $ makeTree dl
