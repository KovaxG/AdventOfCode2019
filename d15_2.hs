{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
-- Advent of Code 2019 Day 15 Part 2
-- https://adventofcode.com/2019/day/15#part2
import IntCodeComp

import qualified Data.Map as Map
import Debug.Trace

type Point = (Int, Int)
data Input = N | S | W | E deriving (Show, Enum, Eq)
data Output = Wall | Ok | Oxy deriving (Show, Enum, Eq)
newtype Map = Map (Map.Map Point Output) deriving (Show)

data State = State {
  comp :: !Computer,
  pos :: !Point,
  dist :: !Int,
  onField :: !Output
} deriving (Show)

foundOxygen :: Map -> Bool
foundOxygen (Map !m) = elem Oxy $ Map.elems m

combine :: Map -> Map -> Map
combine (Map !m1) (Map !m2) = Map $ Map.union m1 m2

addInfo :: (Point, Output) -> Map -> Map
addInfo (!p, !o) (Map !mpo) = Map $ Map.insert p o mpo

apply :: (State, Input) -> (State, (Point, Output))
apply (!s@State{comp}, inp) = (ns, po)
  where
    !inputCode = fromEnum inp + 1
    !compT = runWithInputUntil (\c -> isTerminal c || (not . null . outputs) c) inputCode comp
    !outputCode = head $ outputs $ compT
    !comp' = compT { outputs = [] }
    !output = toEnum outputCode
    !ns = (updateMeta s inp output) { comp = comp' }
    !p' = move inp (pos s)
    !po = (p', output)

updateMeta :: State -> Input -> Output -> State
updateMeta !s@State{pos, dist, onField} i o
  | o == Wall = s
  | otherwise = ns { pos = move i pos }
  where !ns = s { dist = dist + 1, onField = o }

move :: Input -> Point -> Point
move dir (x,y) = case dir of
  N -> (x,y-1)
  S -> (x,y+1)
  W -> (x-1,y)
  E -> (x+1,y)

go :: ([State], Map) -> ([State], Map)
go (!states, !curMap@(Map !innerMap))
  | null states = (states, curMap)
  | otherwise = go $ foldl combinator ([], curMap) states
  where
    combinator :: ([State], Map) -> State -> ([State], Map)
    combinator (ss, m) s =
      let !(ss', m') = lookaround s m
          !states' = ss' ++ ss
      in (filter notFoundInMap states', combine m m')
      where notFoundInMap s = not $ Map.member (pos s) innerMap

lookaround :: State -> Map -> ([State], Map)
lookaround s m = (ss, m')
  where
    !m' = foldl (flip addInfo) m po
    (!ss, !po) = unzip
               $ map (\d -> apply (s, d))
               $ [N .. E]

main :: IO ()
main = do
  comp <- parseInput <$> readFile "d15_1.in"
  let initialState = State comp (0,0) 0 Ok
  let (states, myMap) = go ([initialState], Map $ Map.fromList [((0,0), Ok)])
  putStrLn $ show $ filledWithOxygen myMap

filledWithOxygen :: Map -> Int
filledWithOxygen m = go m 0
  where
    go :: Map -> Int -> Int
    go  (Map m) n
      | length (Map.filter (==Ok) m) == 0 = n
      | otherwise =
        let oxygens = foldl (\m p -> Map.insertWith mixing p Oxy m) m
                    $ (=<<) adjacent
                    $ map fst
                    $ Map.toList
                    $ Map.filter (==Oxy) m
        in go (Map oxygens) (n+1)
      where
        adjacent :: Point -> [Point]
        adjacent (x,y) = [(x-1,y), (x+1,y), (x,y-1),(x,y+1)]

        mixing :: Output -> Output -> Output
        mixing a b = case (a,b) of
            (Oxy, Oxy) -> Oxy
            (Oxy, Wall) -> Wall
            (Oxy, Ok)  -> Oxy
            (Ok, Oxy)  -> Oxy
            (Wall, _)  -> Wall

renderMap :: Map -> String
renderMap (Map m) =
  unlines
  $ chunksOf (maxX - minX + 1)
  $ map toChar
  $ map (flip Map.lookup m)
  $ [(x,y) | y <- [minY .. maxY], x <- [minX .. maxX]]
  where
    points = Map.keys m
    minX = minimum $ map fst points
    maxX = maximum $ map fst points
    minY = minimum $ map snd points
    maxY = maximum $ map snd points
    toChar Nothing = '?'
    toChar (Just Ok) = '.'
    toChar (Just Wall) = '█'
    toChar (Just Oxy) = 'O'

chunksOf :: Int -> [a] -> [[a]]
chunksOf n as
  | length as > n = take n as : chunksOf n (drop n as)
  | otherwise = [as]
