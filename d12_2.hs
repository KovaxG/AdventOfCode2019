{-# LANGUAGE BangPatterns #-}
-- Advent of Code 2019 Day 12 Part 2
-- https://adventofcode.com/2019/day/12#part2
import Text.Parsec
import Data.Either
import Data.List
import Debug.Trace
import System.CPUTime
import qualified Data.Set as Set

type Moon = (Int, Int)

update :: (Moon, Moon, Moon, Moon) -> (Moon, Moon, Moon, Moon)
update !(!m1, !m2, !m3, !m4) =
  let !m1' = updateMoon m1 m2 m3 m4
      !m2' = updateMoon m2 m1 m3 m4
      !m3' = updateMoon m3 m1 m2 m4
      !m4' = updateMoon m4 m1 m2 m3
  in (m1',m2',m3',m4')

updateMoon :: Moon -> Moon -> Moon -> Moon -> Moon
updateMoon (!p1,!v1) (!p2,!v2) (!p3,!v3) (!p4,!v4) =
  let !v = v1 + sig2 p2 p1 + sig2 p3 p1 + sig2 p4 p1
  in (p1+v, v)

sig2 :: Int -> Int -> Int
sig2 !a !b = if a > b then 1 else if a < b then -1 else 0

parseInput :: String -> [(Int, Int, Int)]
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
      return $ (x,y,z)

calc :: (Moon, Moon, Moon, Moon) -> Int
calc !orig = snd $ go (update orig, 1)
  where
    go :: ((Moon, Moon, Moon, Moon), Int)
       -> ((Moon, Moon, Moon, Moon), Int)
    go (!ms, !i)
      | ms == orig = (ms, i)
      | otherwise =
        let returnVal = (update ms, i+1)
        in if mod i 10000000 == 0 then traceShow i $ go returnVal else go returnVal

main :: IO ()
main = do
  t0 <- getCPUTime
  ![(px1,py1,pz1),
    (px2,py2,pz2),
    (px3,py3,pz3),
    (px4,py4,pz4)] <- parseInput <$> readFile "d12_1.in"
  let !xnr = calc ((px1, 0), (px2, 0), (px3, 0), (px4, 0))
  let !ynr = calc ((py1, 0), (py2, 0), (py3, 0), (py4, 0))
  let !znr = calc ((pz1, 0), (pz2, 0), (pz3, 0), (pz4, 0))
  t1 <- getCPUTime
  putStrLn $ "x: " ++ show xnr
  putStrLn $ "y: " ++ show ynr
  putStrLn $ "z: " ++ show znr
  putStrLn $ show $ foldl lcm 1 [xnr, ynr, znr]
  putStrLn $ show (div (t1 - t0) 1000000) ++ " us"
