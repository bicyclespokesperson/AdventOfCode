import qualified Data.List as L
import System.IO

data Shape
  = Rock
  | Paper
  | Scissors
  deriving (Bounded, Eq, Enum, Show)

data Outcome
  = Win
  | Draw
  | Loss
  deriving (Show, Eq)

class (Eq a, Enum a, Bounded a) =>
      CyclicEnum a
  where
  cpred :: a -> a
  cpred c
    | c == minBound = maxBound
    | otherwise = pred c
  csucc :: a -> a
  csucc c
    | c == maxBound = minBound
    | otherwise = succ c

instance CyclicEnum Shape

-- This could be read?
toShape :: Char -> Shape
toShape c
  | c == 'A' = Rock
  | c == 'B' = Paper
  | c == 'C' = Scissors

toOutcome :: Char -> Outcome
toOutcome c
  | c == 'X' = Loss
  | c == 'Y' = Draw
  | c == 'Z' = Win

battle :: Shape -> Shape -> Outcome
battle playerShape oppShape
  | playerShape == oppShape = Draw
  | cpred playerShape == oppShape = Win
  | otherwise = Loss

inverseBattle :: Shape -> Outcome -> Shape
inverseBattle oppShape result
  | result == Win = csucc oppShape
  | result == Loss = cpred oppShape
  | otherwise = oppShape

scoreSh :: Shape -> Int
scoreSh Rock = 1
scoreSh Paper = 2
scoreSh Scissors = 3

scoreOt :: Outcome -> Int
scoreOt Loss = 0
scoreOt Draw = 3
scoreOt Win = 6

score :: Shape -> Outcome -> Int
score shp result = scoreSh shp + scoreOt result

lineToShapeResult :: String -> (Shape, Outcome)
lineToShapeResult l =
  (toShape (head l), toOutcome (head (dropWhile (== ' ') (tail l))))

readInputData :: String -> IO [(Shape, Outcome)]
readInputData filename = do
  moveList <- lines <$> readFile filename
  return (map lineToShapeResult moveList)

main :: IO ()
main = do
  battles <- readInputData "input.txt"
  print $
    sum $
    map
      (\(oppChoice, result) -> score (inverseBattle oppChoice result) result)
      battles
