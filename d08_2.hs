-- Advent of Code 2019 Day 8 Part 2
-- https://adventofcode.com/2019/day/8#part2
import qualified Data.Map as Map

data Pixel = Transparent
           | White
           | Black
           deriving (Show)

instance Semigroup Pixel where
  Transparent <> White = White
  Transparent <> Black = Black
  other <> _ = other

instance Monoid Pixel where
  mempty = Transparent

data Image = Image (Map.Map Int Pixel)

instance Semigroup Image where
  (Image m1) <> (Image m2) = Image $ Map.unionWith (<>) m1 m2

instance Monoid Image where
  mempty = toImage
         $ replicate (width * height) Transparent

chunksOf :: Int -> [a] -> [[a]]
chunksOf n as
  | length as > n = take n as : chunksOf n (drop n as)
  | otherwise = [as]

parseChar :: Char -> Pixel
parseChar n = case n of
  '0' -> Black
  '1' -> White
  '2' -> Transparent

toChar :: Pixel -> Char
toChar c = case c of
  Black -> '0'
  White -> '1'
  Transparent -> '2'

instance Show Image where
  show (Image ps) = map toChar $ Map.elems ps

width  = 25
height = 6

toImage :: [Pixel] -> Image
toImage ps = Image
           $ Map.fromList
           $ zip [0 .. width * height] ps

makeReadable :: Char -> Char
makeReadable '1' = '█'
makeReadable '\n' = '\n'
makeReadable _ = ' '

main:: IO ()
main = do
  contents <- readFile "d08_1.in"
  putStrLn $ map makeReadable
           $ unlines
           $ chunksOf width
           $ show
           $ mconcat
           $ map (toImage . map parseChar)
           $ chunksOf (width * height)
           $ contents
