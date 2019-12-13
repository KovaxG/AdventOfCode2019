-- Advent of Code 2019 Day 13 Part 1
-- https://adventofcode.com/2019/day/13
import IntCodeComp

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

data Tile = Empty | Wall | Block | HPaddle | Ball deriving (Show, Eq)
type Point = (Int, Int)

toTile :: Int -> Tile
toTile tileId = case tileId of
  0 -> Empty
  1 -> Wall
  2 -> Block
  3 -> HPaddle
  4 -> Ball

data Screen = Screen {
  tiles :: Map.Map Point Tile
} deriving (Show)

data Cabinet = Cabinet {
  computer :: Computer,
  screen :: Screen
} deriving (Show)

updateScreen :: [Int] -> Screen -> Screen
updateScreen [x, y, tileId] (Screen ts) =
  Screen $ Map.insert (x,y) (toTile tileId) ts

tileToChr :: Tile -> Char
tileToChr t = case t of
  Empty -> '.'
  Wall -> '#'
  Block -> '?'
  HPaddle -> '-'
  Ball -> 'o'

main :: IO ()
main = do
  comp <- parseInput <$> readFile "d13_1.in"
  let screenCode = outputs $ runUntil isTerminal comp
  let screen = foldl (flip updateScreen)
                     (Screen (Map.fromList []))
                     (chunksOf 3 screenCode)
  putStrLn $ drawMap tileToChr Empty $ tiles screen
  putStrLn $ show $ count Block $ Map.elems $ tiles screen

drawMap :: (a -> Char) -> a -> Map.Map Point a -> String
drawMap draw def as =
  let keys = Map.keys as
      minX = minimum $ map fst keys
      maxX = maximum $ map fst keys
      minY = minimum $ map snd keys
      maxY = maximum $ map snd keys
  in unlines
     $ chunksOf (maxX - minX + 1)
     $ map (draw . Maybe.fromMaybe def . flip Map.lookup as)
     $ [(x,y) | x <- [minX .. maxX], y <- [minY .. maxY] ]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n as
 | length as > n = take n as : chunksOf n (drop n as)
 | otherwise = [as]

count :: (Eq a) => a -> [a] -> Int
count a = length . filter (==a)
