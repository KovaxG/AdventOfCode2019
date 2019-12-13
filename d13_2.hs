{-# LANGUAGE BangPatterns #-}
-- Advent of Code 2019 Day 13 Part 2
-- https://adventofcode.com/2019/day/13#part2
import IntCodeComp

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Debug.Trace

data Tile = Empty | Wall | Block | HPaddle | Ball deriving (Show, Eq)
type Point = (Int, Int)

toTile :: Int -> Tile
toTile !tileId = case tileId of
  0 -> Empty
  1 -> Wall
  2 -> Block
  3 -> HPaddle
  4 -> Ball

data Screen = Screen {
  tiles :: !(Map.Map Point Tile),
  segDisp :: !Int
} deriving (Show, Eq)

data Cabinet = Cabinet {
  computer :: !Computer,
  screen :: !Screen
} deriving (Show)

updateScreen :: [Int] -> Screen -> Screen
updateScreen [!x, !y, !tileId] !(Screen !ts !s)
  | x == -1 && y == 0 = Screen ts tileId
  | otherwise = Screen (Map.insert (x,y) (toTile tileId) ts) s

tileToChr :: Tile -> Char
tileToChr t = case t of
  Empty -> '.'
  Wall -> '█'
  Block -> '#'
  HPaddle -> '█'
  Ball -> 'o'

getBallPosition :: Screen -> Point
getBallPosition (Screen ts _) = Maybe.fromMaybe (0,0)
                              $ fmap fst
                              $ List.find (\(p,t) -> t == Ball)
                              $ Map.toList ts

getPaddlePosition :: Screen -> Point
getPaddlePosition (Screen ts _) = Maybe.fromMaybe (0,0)
                              $ fmap fst
                              $ List.find (\(p,t) -> t == HPaddle)
                              $ Map.toList ts

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
     $ [(x,y) | y <- [minY .. maxY], x <- [minX .. maxX]  ]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n as
 | length as > n = take n as : chunksOf n (drop n as)
 | otherwise = [as]

count :: (Eq a) => a -> [a] -> Int
count a = length . filter (==a)

parse :: String -> Int
parse ('a':_) = -1
parse ('d':_) =  1
parse _ = 0

gameLoop :: Cabinet -> IO ()
gameLoop s = do
  putStrLn $ drawMap tileToChr Empty $ tiles $ (screen s)
  putStrLn $ "Score: " ++ show (segDisp (screen s))
  input <- parse <$> (return "1")
  let comp = runUntil (\c -> isTerminal c || (null . inputs) c)
           $ (computer s) { inputs = [input] }
  let screenCode = outputs comp
  let scr = foldl (flip updateScreen) (screen s) (chunksOf 3 screenCode)
  putStrLn "------------------------------------------------------------"
  gameLoop (s { computer = comp { outputs = [] }, screen = scr }) (bx1, by1)

score :: Cabinet -> Int
score = segDisp . screen

blocks :: Cabinet -> Int
blocks = count Block . Map.elems . tiles . screen

updateCab :: Int -> Cabinet -> Maybe Cabinet
updateCab !input !cab =
  let !comp = runUntil (\c -> isTerminal c || (null . inputs) c)
           $ (computer cab) { inputs = [input] }
  in if null (outputs comp) || snd (getBallPosition (screen cab)) >=23
     then Nothing
     else let !scrCmd = outputs comp
              !scr = foldl (flip updateScreen) (screen cab) (chunksOf 3 scrCmd)
          in Just $ cab { computer = comp { outputs = [] }, screen = scr }


blankScreen = Screen (Map.fromList [((0,0), Empty)]) 0

main :: IO ()
main = do
  !comp <- writeMemory 0 2 . parseInput <$> readFile "d13_2.in"
  let !initialState = Cabinet { computer = comp, screen = blankScreen }
  gameLoop initialState
