{-# LANGUAGE BangPatterns #-}
-- Advent of Code 2019 Day 14 Part 1
-- https://adventofcode.com/2019/day/14
import Text.Parsec
import Data.Either
import Data.Maybe
import Data.List
import Debug.Trace

type Ore = Int
type Label = String
type Component = (Int, Label)
data Rule = Rule ![Component] !Component deriving (Show)

reduceToOre :: [Rule] -> Int
reduceToOre !rs = go rs (0, [(1, "FUEL")])

go :: [Rule] -> (Ore, [Component]) -> Int
go !rs (!ore, !cs)
  | null cs = ore
  | any (canApplyRule rs) cs =
    let (o, ls) = foldl (\(o, lc) c ->
                          let (o1, lc1) = applyLossLessRule rs c
                          in (o + o1, lc ++ lc1)) (0, []) cs
    in traceShow (ore + o, combine ls) $ go rs (ore + o, combine ls)
  | otherwise =
    -- TODO bleah
    let !components = map (\ls -> let ore = sum $ map fst ls
                                      cons = filter (/= cs) $ combine $ map snd ls
                                  in (ore, cons))
                    $ tryAll (applyLossLessRule rs . snd)
                    $ cs
    in minimum $ map (go rs) components

canApplyRule :: [Rule] -> Component -> Bool
canApplyRule !rs (!n, !l) = div n m > 0
 where Rule _ (m, _) = productionRuleOf rs l

applyLossLessRule :: [Rule] -> Component -> (Ore, [Component])
applyLossLessRule !rs !c@(!n,!l) =
  let Rule cs (m, _) = productionRuleOf rs l
      multiplier = div n m
      remaining = n - multiplier * m
      constituents = map (\(val, lab) -> (val * multiplier, lab)) cs
      ore = sum $ map fst $ filter ((=="ORE") . snd) constituents
      cons = filter ((/="ORE") . snd) constituents
      newComponents = filter ((/=0) . fst) $ cons ++ [(remaining, l)]
  in (ore, newComponents)

combine :: [Component] -> [Component]
combine = map (\g -> (sum $ map fst g, snd $ head g))
        . groupBy (\a b -> snd a == snd b)
        . sortBy (\a b -> compare (snd a) (snd b))

productionRuleOf :: [Rule] -> Label -> Rule
productionRuleOf !rs !c =
  fromMaybe (Rule [] (1,c))
  $ listToMaybe
  $ filter (\(Rule _ (_, l)) -> l == c) rs

parseRules :: String -> [Rule]
parseRules = fromRight undefined . parse (many (rule <* newline)) ""
  where
    component = do
      number <- read <$> many digit
      spaces
      label <- many alphaNum
      return (number, label)

    rule = do
      components <- sepBy component (char ',' >> spaces)
      spaces
      string "=>"
      spaces
      result <- component
      return $ Rule components result

readFromFile :: IO [Rule]
readFromFile = parseRules <$> readFile "d14_1.in"

main :: IO ()
main = do
  rules <- readFromFile
  putStrLn $ show $ reduceToOre rules

tryAll :: (a -> [a]) -> [a] -> [[a]]
tryAll f !as = map (\(i, ls) ->
                     let (h, m, s) = mySplit i ls
                     in h ++ f m ++ s )
            $ zip [0 ..]
            $ replicate (length as) as

mySplit :: Int -> [a] -> ([a], a, [a])
mySplit !n !as = (f, m, s)
  where (f, m:s) = splitAt n as
