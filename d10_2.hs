-- Advent of Code 2019 Day 10 Part 2
-- https://adventofcode.com/2019/day/10#part2
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe

type Point = (Int, Int)
data Ray = Ray Point Point deriving (Show)
data Direction = Dir Rational deriving (Show, Eq, Ord)

instance Eq Ray where
  r1@(Ray o1 p1) == r2@(Ray o2 p2) =
    o1 == o2 && (contains p2 r1 || contains p1 r2 || p1 == p2)

angleOf :: Ray -> Double
angleOf (Ray p1@(x1,y1) p2@(x2,y2))
  | y < 0 && x >= 0 = - atan (x/y)
  | y >= 0 && x > 0 = (pi/2) + atan (y/x)
  | y > 0 && x <= 0 = pi - atan (x/y)
  | y <= 0 && x < 0 = (3*pi/2) + atan (y/x)
  where
    l = dist p1 p2
    x = fromIntegral (x2 - x1)
    y = fromIntegral (y2 - y1)

contains :: Point -> Ray -> Bool
contains (x,y) r@(Ray (x0, y0) (x1, y1))
  | x1 == x0  = x == x0 && if y1 > y0 then y > y1 else y < y1
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

generateOrder :: [[Ray]] -> [Point]
generateOrder = reverse . fst . go . (,) []
  where
    go :: ([Point], [[Ray]]) -> ([Point], [[Ray]])
    go (ps, []) = (ps, [])
    go (ps, rs:rest)
      | length rs == 1 =
        let (Ray _ p) = head rs
        in go (p : ps, rest)
      | otherwise =
        let (Ray _ p) = head rs
        in go (p : ps, rest ++ [tail rs])

main :: IO ()
main = do
  asteroids <- parseInput <$> readFile "d10_1.in"
  let base = (26,29)
  let rays = List.group
           $ List.sortBy (\a b -> compare (angleOf a) (angleOf b))
           $ map (Ray base)
           $ List.sortBy (\a b -> compare (dist base a) (dist base b))
           $ Set.toList
           $ Set.delete base asteroids
  putStrLn $ show
           $ head
           $ map (\(_, (x,y)) -> x * 100 + y)
           $ filter ((==200) . fst)
           $ zip [1 ..]
           $ generateOrder
           $ rays
