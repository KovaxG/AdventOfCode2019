-- Advent of Code 2019 Day 3 Part 2
-- https://adventofcode.com/2019/day/3#part2
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Char

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

makePath :: [Section] -> Map.Map Point Int
makePath = snd . snd . foldl rule ((0,0), (1, Map.empty))
  where
    rule :: (Point, (Int, Map.Map Point Int))
         -> Section
         -> (Point, (Int, Map.Map Point Int))
    rule ((x,y), (len, pointMap)) s = case s of
      R n -> ((x + n, y), insertAndMeasure $ zip [x + 1 .. x + n] (repeat y))
      L n -> ((x - n, y), insertAndMeasure $ zip (reverse [x - n .. x - 1]) (repeat y))
      U n -> ((x, y - n), insertAndMeasure $ zip (repeat x) (reverse [y - n .. y - 1]))
      D n -> ((x, y + n), insertAndMeasure $ zip (repeat x) [y + 1 .. y + n])
      where
          insertAndMeasure :: [Point] -> (Int, Map.Map Point Int)
          insertAndMeasure newPoints =
            foldl (\(l, ps) p -> (l + 1, Map.insert p l ps))
                  (len, pointMap)
                  newPoints

main :: IO ()
main = do
  [s1, s2] <- map (Map.delete (0,0) . makePath . parsePath) -- TODO remove (0,0)
             . lines
             <$> readFile "d03_1.in"
  putStrLn $ show
           $ minimum
           $ map (\(a,b) -> a + b)
           $ Map.elems
           $ Map.intersectionWith (,) s1 s2

-- myPrint :: Map.Map Point Int -> IO ()
-- myPrint mpi =
--   mapM_ (putStrLn . show)
--         $ chunksOf (length [-10 .. 10])
--         $ map (maybe ' ' toNum)
--         $ map (flip Map.lookup mpi)
--         $ [(x,y) | x <- [-10 .. 10], y <- [-10 .. 10]]
--
-- chunksOf :: Int -> [a] -> [[a]]
-- chunksOf n as
--   | length as >= n = take n as : chunksOf n (drop n as)
--   | otherwise = []
--
-- toNum :: Int -> Char
-- toNum n
--   | 0 <= n && n <= 9 = head $ show n
--   | otherwise = chr (55 + n)
