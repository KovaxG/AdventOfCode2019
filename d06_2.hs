-- Advent of Code 2019 Day 6 Part 2
-- https://adventofcode.com/2019/day/6#part2
import Data.Tree
import Data.List

makeTree :: (Int, String) -> [(String, String)] -> Tree (Int, String)
makeTree (distanceFromRoot, currentValue) connectionList =
  Node (distanceFromRoot, currentValue)
       $ map (\(_, childValue) ->
                makeTree (distanceFromRoot + 1, childValue) connectionList'
             )
             relevantConnections
  where
    connectionList' = filter (\(_,b) -> b /= currentValue) connectionList
                      \\ relevantConnections
    relevantConnections = filter (\(a,_) -> a == currentValue) connectionList

parseString :: String -> [(String, String)]
parseString = map ((\[a,b] -> (a,b)) . words . repSep) . lines
  where repSep = map (\c -> if c == ')' then ' ' else c)

main :: IO ()
main = do
  directedConnections <- parseString <$> readFile "d06_1.in"
  let biDirectionalConnections = directedConnections ++ map (\(a,b) -> (b,a)) directedConnections
  putStrLn $ show
           $ (\(Just (i,_)) -> i-2)
           $ find (\(_, s) -> s == "SAN")
           $ flatten -- basically toList
           $ makeTree (0, "YOU") biDirectionalConnections
