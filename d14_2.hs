{-# LANGUAGE BangPatterns #-}
-- Advent of Code 2019 Day 14 Part 2
-- https://adventofcode.com/2019/day/14#part2
import Text.Parsec
import Data.Either
import Data.Maybe
import Data.List
import Debug.Trace

type Ore = Int
type Label = String
type Component = (Int, Label)
data Rule = Rule ![Component] !Component deriving (Show)

reduceToOre :: [Rule] -> Component -> Int
reduceToOre !rs !c = go rs (0, [c])

go :: [Rule] -> (Ore, [Component]) -> Int
go !rs (!ore, !cs)
  | null cs = ore
  | any (canApplyRule rs) cs =
    let (o, ls) = applyList (lossLessRule rs) (0, []) cs
    in go rs (ore + o, combine ls)
  | otherwise =
    let labels = map snd cs
        waitList = nub $ labels >>= (allComponents rs)
        necessary = labels \\ waitList
        necessaryComps = filter (flip elem necessary . snd) cs
        waitListComps = filter (flip elem waitList . snd) cs
        (cucc, cuccok) = if null necessary
                         then applyList (lossyRule rs) (ore, []) cs
                         else applyList (lossyRule rs) (ore, waitListComps) necessaryComps
    in go rs (cucc, combine cuccok)

applyList :: (Component -> (Ore, [Component]))
          -> (Ore, [Component])
          -> [Component]
          -> (Ore, [Component])
applyList rule starting comps =
  foldl (\(o, lc) c -> let (o1, lc1) = rule c in (o+o1, lc ++ lc1)) starting comps

canApplyRule :: [Rule] -> Component -> Bool
canApplyRule !rs (!n, !l) = div n m > 0
 where Rule _ (m, _) = productionRuleOf rs l

lossLessRule :: [Rule] -> Component -> (Ore, [Component])
lossLessRule !rs !c@(!n,!l) =
  let Rule cs (m, _) = productionRuleOf rs l
      multiplier = div n m
      remaining = n - multiplier * m
      constituents = map (\(val, lab) -> (val * multiplier, lab)) cs
      ore = sum $ map fst $ filter ((=="ORE") . snd) constituents
      cons = filter ((/="ORE") . snd) constituents
      newComponents = filter ((/=0) . fst) $ cons ++ [(remaining, l)]
  in (ore, newComponents)

lossyRule :: [Rule] -> Component -> (Ore, [Component])
lossyRule !rs !c@(!n,!l) =
  let Rule cs (m, _) = productionRuleOf rs l
      multiplier = div n m + signum (mod n m)
      constituents = map (\(val, lab) -> (val * multiplier, lab)) cs
      ore = sum $ map fst $ filter ((=="ORE") . snd) constituents
      cons = filter ((/="ORE") . snd) constituents
  in (ore, cons)

allComponents :: [Rule] -> Label -> [Label]
allComponents rs l = (go [l]) \\ [l]
  where
    go :: [Label] -> [Label]
    go ls = if null components then ls else nub $ ls ++ go (components)
      where
        components = componentsOf =<< ls

        componentsOf :: Label -> [Label]
        componentsOf l =
          let Rule cs _ = productionRuleOf rs l
          in filter (/="ORE") $ map snd cs

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

main :: IO ()
main = do
  rules <- parseRules <$> readFile "d14_1.in"
  putStrLn $ show $ guess rules 1 trillion

trillion = 1000000000000

guess :: [Rule] -> Int -> Int -> Int
guess rs smallGuess bigGuess
  | bigGuess - smallGuess == 1 = smallGuess
  | oreRequired < trillion = guess rs midGuess bigGuess
  | oreRequired > trillion = guess rs smallGuess midGuess
  where
    midGuess = div (smallGuess + bigGuess) 2
    oreRequired = reduceToOre rs (midGuess, "FUEL")
