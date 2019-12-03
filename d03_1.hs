-- Advent of Code 2019 Day 3 Part 1
-- https://adventofcode.com/2019/day/3
import qualified Data.List as List
import qualified Data.Set as Set

data Section = R Int | L Int | U Int | D Int deriving (Show, Read)
type Point = (Int, Int)

parsePath :: String -> [Section]
parsePath =
  map parseSection
  . words
  . map (\c -> if c == ',' then ' ' else c)

parseSection :: String -> Section
parseSection (c:cs) = read $ c : ' ' : cs

dist :: Point -> Point -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

makePath :: [Section] -> Set.Set Point
makePath = snd . foldl rule ((0,0), Set.empty)
  where
    rule :: (Point, Set.Set Point) -> Section -> (Point, Set.Set Point)
    rule ((x,y), ps) s = case s of
      R n -> ((x + n, y), foldl (flip Set.insert) ps $ zip [x .. x + n] (repeat y))
      L n -> ((x - n, y), foldl (flip Set.insert) ps $ zip [x - n .. x] (repeat y))
      U n -> ((x, y - n), foldl (flip Set.insert) ps $ zip (repeat x) [y - n .. y])
      D n -> ((x, y + n), foldl (flip Set.insert) ps $ zip (repeat x) [y .. y + n])

main :: IO ()
main = do
  [s1, s2] <- map ((Set.\\ (Set.fromList [(0,0)])) . makePath . parsePath)
             . lines
             <$> readFile "d03_1.in"
  putStrLn $ show
           $ List.minimumBy (\a b -> compare (snd a) (snd b))
           $ map (\p -> (p, dist (0,0) p))
           $ Set.toList
           $ Set.intersection s1 s2
