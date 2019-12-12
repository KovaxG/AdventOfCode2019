-- Advent of Code Day 10 Part 1
-- https://adventofcode.com/2019/day/10
import qualified Data.Set as Set
import qualified Data.List as List

type Point = (Int, Int)
data Ray = Ray Point Point deriving (Show)

contains :: Point -> Ray -> Bool
contains (x,y) (Ray (x0, y0) (x1, y1))
  | x1 - x0 == 0 = x == x0 && if y1 > y0 then y > y1 else y < y1
  | otherwise =
    let m = fromIntegral (y1 - y0) / fromIntegral (x1 - x0) :: Rational
        b = fromIntegral y0 - m * fromIntegral x0 :: Rational
    in fromIntegral y == m * fromIntegral x + b
       && if x1 > x0 then x > x1 else x < x1

parseInput :: String -> Set.Set Point
parseInput = Set.fromList
           . map fst
           . filter (\(_, c) -> c /= '.')
           . (=<<) (\(y, l) -> map (\(x, c) -> ((x,y),c))
                             $ zip [0 ..] l)
           . zip [0 ..]
           . lines

countVisible :: Point -> Set.Set Point -> (Point, Int)
countVisible origin =
  (,) origin
  . length
  . snd
  . foldl rule ([], [])
  . List.sortBy (\a b -> compare (dist origin a) (dist origin b))
  . filter (/=origin)
  . Set.toList
  where
    rule :: ([Point], [Ray]) -> Point -> ([Point], [Ray])
    rule (ps, ls) p
      | any (contains p) ls = (ps, ls)
      | otherwise = (p : ps, (Ray origin p) : ls)

dist :: Point -> Point -> Double
dist (x1,y1) (x2,y2) =
  sqrt $ (fromIntegral x1 - fromIntegral x2)^2
       + (fromIntegral y1 - fromIntegral y2)^2

main :: IO ()
main = do
  asteroids <- parseInput <$> readFile "d10_1.in"
  putStrLn $ show
           $ List.maximumBy (\a b -> compare (snd a) (snd b))
           $ map (flip countVisible asteroids)
           $ Set.toList asteroids
