{-# LANGUAGE BangPatterns #-}
-- Advent of Code 2019 Day 12 Part 2
-- https://adventofcode.com/2019/day/12#part2
import Text.Parsec
import Data.Either
import Data.List
import Debug.Trace
import System.CPUTime
import qualified Data.Set as Set

type Moon = (Int, Int, Int, Int, Int, Int)

update :: (Moon, Moon, Moon, Moon) -> (Moon, Moon, Moon, Moon)
update !(!m1, !m2, !m3, !m4) =
  let !m1' = updateMoon m1 m2 m3 m4
      !m2' = updateMoon m2 m1 m3 m4
      !m3' = updateMoon m3 m1 m2 m4
      !m4' = updateMoon m4 m1 m2 m3
  in (m1',m2',m3',m4')

updateMoon :: Moon -> Moon -> Moon -> Moon -> Moon
updateMoon (!px1,!py1,!pz1,!vx1,!vy1,!vz1)
           (!px2,!py2,!pz2,!vx2,!vy2,!vz2)
           (!px3,!py3,!pz3,!vx3,!vy3,!vz3)
           (!px4,!py4,!pz4,!vx4,!vy4,!vz4) =
  let !vx = vx1 + sig2 px2 px1 + sig2 px3 px1 + sig2 px4 px1
      !vy = vy1 + sig2 py2 py1 + sig2 py3 py1 + sig2 py4 py1
      !vz = vz1 + sig2 pz2 pz1 + sig2 pz3 pz1 + sig2 pz4 pz1
  in (px1+vx, py1+vy, pz1+vz, vx, vy, vz)

sig2 :: Int -> Int -> Int
sig2 !a !b = if a > b then 1 else if a < b then -1 else 0

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
      return $ (x,y,z, 0,0,0)

calc :: (Moon, Moon, Moon, Moon) -> Int
calc !orig = (\(_, _, i) -> i) $ go (Set.fromList [orig], update orig, 1)
  where
    go :: (Set.Set (Moon, Moon, Moon, Moon), (Moon, Moon, Moon, Moon), Int)
       -> (Set.Set (Moon, Moon, Moon, Moon), (Moon, Moon, Moon, Moon), Int)
    go (!ps, !ms, !i)
      | Set.member ms ps = traceShow ps $ (ps, ms, i)
      | otherwise =
        let ms' = update ms
            ps' = Set.insert ms' ps
        in if mod i 10000000 == 0
           then traceShow i $ go (ps', ms', i+1)
           else go (ps', ms', i+1)

main :: IO ()
main = do
  t0 <- getCPUTime
  ![m1, m2, m3, m4] <- parseInput <$> readFile "d12_1.in"
  let !nr = calc (m1,m2,m3,m4)
  t1 <- getCPUTime
  putStrLn $ show nr
  writeFile "d12_1.out" $ ("The answer is: " ++ show nr)
  putStrLn $ show (div (t1 - t0) 1000000) ++ " us"
