-- Advent of Code 2019 Day 12 Part 1
-- https://adventofcode.com/2019/day/12
import Text.Parsec
import Data.Either

type Triple = (Int, Int, Int)
type Position = Triple
type Velocity = Triple
data Moon = Moon Position Velocity deriving (Show, Eq)

update :: [Moon] -> [Moon]
update moons = map updatePosition
             $ map (\m -> updateVelocity m (filter (/=m) moons))
             $ moons

initial :: [Moon]
initial =
  [ Moon (-1,  0, 2) noVel
  , Moon ( 2,-10,-7) noVel
  , Moon ( 4, -8, 8) noVel
  , Moon ( 3,  5,-1) noVel ]
  where
    noVel = (0,0,0)

totalEnergy :: [Moon] -> Int
totalEnergy = sum . map energy

energy :: Moon -> Int
energy (Moon (px,py,pz) (vx,vy,vz)) = potential * kinetic
  where
    potential = abs px + abs py + abs pz
    kinetic   = abs vx + abs vy + abs vz

updatePosition :: Moon -> Moon
updatePosition (Moon (px,py,pz) v@(vx,vy,vz)) =
  Moon (px+vx,py+vy,pz+vz) v

updateVelocity :: Moon -> [Moon] -> Moon
updateVelocity m = foldl gravity m

gravity :: Moon -> Moon -> Moon
gravity (Moon p@(px1,py1,pz1) (vx, vy, vz))
      (Moon (px2,py2,pz2) _) =
      let vx' = vx + signum (px2 - px1)
          vy' = vy + signum (py2 - py1)
          vz' = vz + signum (pz2 - pz1)
      in Moon p (vx', vy', vz')

parseInput :: String -> [Moon]
parseInput = fromRight undefined . parse (many line) ""
  where
    line = do
      string "<x="
      x <- read <$> many (char '-' <|> digit)
      string ", y="
      y <- read <$> many (char '-' <|> digit)
      string ", z="
      z <- read <$> many (char '-' <|> digit)
      string ">\n"
      return $ Moon (x,y,z) (0,0,0)

main :: IO ()
main = do
  moons <- parseInput <$> readFile "d12_1.in"
  putStrLn $ show
           $ last
           $ map totalEnergy
           $ take 1001
           $ iterate update moons
