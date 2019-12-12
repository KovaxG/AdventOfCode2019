{-# LANGUAGE NamedFieldPuns #-}
-- Advent of Code 2019 Day 11 Part 2
-- https://adventofcode.com/2019/day/11#part2
import IntCodeComp

import Prelude hiding (Left, Right)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List

type Point = (Int, Int)
data Color = Black | White deriving (Show, Eq)
data RobotInstruction = TurnRight | TurnLeft deriving (Show)
data Direction = Up | Down | Left | Right deriving (Show)

data State = State {
  computer :: Computer,
  robot :: Point,
  direction :: Direction,
  hull :: Map.Map Point Color
} deriving (Show)

mkState :: Computer -> State
mkState computer =
  State { computer, robot = (0,0), hull = Map.fromList [((0,0), White)], direction = Up }

move :: Point -> Direction -> Point
move (x,y) Up = (x, y-1)
move (x,y) Down = (x, y+1)
move (x,y) Left = (x-1, y)
move (x,y) Right = (x+1, y)

rotate :: RobotInstruction -> Direction -> Direction
rotate TurnLeft dir = case dir of
  Up -> Left
  Down -> Right
  Left -> Down
  Right -> Up
rotate TurnRight dir = case dir of
  Up -> Right
  Down -> Left
  Left -> Up
  Right -> Down

haltCondition :: Computer -> Bool
haltCondition c = isTerminal c || hasTwoOutputs c
  where
    hasTwoOutputs = (==2) . length . outputs

update :: State -> State
update s@State {robot, hull, computer, direction} =
  let color = readPanel hull robot
      input = toInput color
      computerO = runWithInputUntil haltCondition input computer
  in if isTerminal computerO
     then s { computer = computerO }
     else
        let [colorCode, instructionCode] = outputs computerO
            computer' = computerO { outputs = [] }
            newColor = fromOutput colorCode
            hull' = paintPanel robot newColor hull
            instruction = toRobotInstruction instructionCode
            (robot', direction') = execute instruction (robot, direction)
        in State { computer = computer'
                 , hull = hull'
                 , robot = robot'
                 , direction = direction' }
  where
    execute :: RobotInstruction -> (Point, Direction) -> (Point, Direction)
    execute instruction (pos ,dir) =
      let dir' = rotate instruction dir
          pos' = move pos dir'
      in (pos', dir')

    toRobotInstruction :: Int -> RobotInstruction
    toRobotInstruction 0 = TurnLeft
    toRobotInstruction 1 = TurnRight

    fromOutput :: Int -> Color
    fromOutput 0 = Black
    fromOutput 1 = White

    toInput :: Color -> Int
    toInput Black = 0
    toInput White = 1

    readPanel :: Map.Map Point Color -> Point -> Color
    readPanel ps p = Maybe.fromMaybe Black $ Map.lookup p ps

    paintPanel :: Point -> Color -> Map.Map Point Color -> Map.Map Point Color
    paintPanel p c ps = Map.insert p c ps

isFinished :: State -> Bool
isFinished State {computer} = isTerminal computer

paintHull :: State -> (State -> State) -> State
paintHull original update
  | isFinished original = original
  | otherwise = paintHull (update original) update

main :: IO ()
main = do
  comp <- parseInput <$> readFile "d11_1.in"
  let myHull = hull $ paintHull (mkState comp) update
  putStrLn $ show $ maximum $ map fst $ Map.keys myHull
  putStrLn $ drawHull myHull


drawHull :: Map.Map Point Color -> String
drawHull ps =
  let points = Map.keys ps
      minX = minimum $ map fst points
      maxX = maximum $ map fst points
      minY = minimum $ map snd points
      maxY = maximum $ map snd points
  in unlines
     $ chunksOf (maxX - minX + 1)
     $ map (\c -> if c == Black then '.' else '█')
     $ map (Maybe.fromMaybe Black . flip Map.lookup ps)
     $ [(x,y) | y <- [minY .. maxY], x <- [minX .. maxX]]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n as
  | length as > n = take n as : chunksOf n (drop n as)
  | otherwise = [as]
