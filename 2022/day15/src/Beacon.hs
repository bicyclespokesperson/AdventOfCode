module Main where

-- Inspired by https://mmhaskell.com/blog/2022/12/15/day-15-beacons-and-scanners

import Data.Void (Void)
import Data.List (nub, sort)
import Data.Maybe (mapMaybe, fromJust, isJust)
import qualified Data.Array as A
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as LL (decimal, signed)

type Parser = Parsec Void String


type Coord = (Int, Int)
type Interval = (Int, Int)

parseCoord :: Parser Coord
parseCoord = do
  _ <- string "x="
  x <- LL.signed space LL.decimal
  _ <- string ", y="
  y <- LL.signed space LL.decimal
  return (x, y)

parseLine :: Parser (Coord, Coord)
parseLine = do
  _ <- string "Sensor at "
  sensorCoords <- parseCoord
  _ <- string ": closest beacon is at "
  beaconCoords <- parseCoord
  _ <- newline
  return (sensorCoords, beaconCoords)

parseCoordPairs :: Parsec Void String [(Coord, Coord)]
parseCoordPairs = many parseLine

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

getNeighbors :: Coord -> [Coord]
getNeighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

excludedCoords :: Int -> (Coord, Coord) -> Maybe Interval
excludedCoords rowNum (sensor@(sx, sy), beacon) = let dist = manhattanDistance sensor beacon
                                                      leftover = dist - abs (sy - rowNum)
                                                   in if leftover < 0 then Nothing else Just (sx - leftover, sx + leftover)

mergeIntervals :: [Interval] -> [Interval]
mergeIntervals a = mergeIntervalsHelper (head a) (tail a) []
  where
    mergeIntervalsHelper :: Interval -> [Interval] -> [Interval] -> [Interval]
    mergeIntervalsHelper current@(c1, c2) (next@(n1, n2):xs) acc = if c2 >= n1
                                                                    then mergeIntervalsHelper (c1, max n2 c2) xs acc
                                                                    else mergeIntervalsHelper next xs (current:acc)
    mergeIntervalsHelper current [] acc = reverse (current:acc)

-- Binary search a Data.Array.
-- Unused but maybe useful to have around at some point.
binarySearch :: (Coord -> Coord -> Ordering) -> Int -> Int -> A.Array Int Coord -> Coord -> Maybe Int
binarySearch cmp lo hi arr val
  | lo > hi   = Nothing
  | otherwise =
      let mid = (lo + hi) `div` 2
          midVal = arr A.! mid
      in case cmp val midVal of
        LT -> binarySearch cmp lo (mid - 1) arr val
        EQ -> Just mid
        GT -> binarySearch cmp (mid + 1) hi arr val

countIntervalsExcludingBeacons :: [Interval] -> [Int] -> Int
countIntervalsExcludingBeacons intervals beaconXs = countTail 0 intervals (sort beaconXs)
  where
    countTail :: Int -> [Interval] -> [Int] -> Int
    countTail accum [] _ = accum
    countTail accum ((next1, next2) : rest) [] = countTail (accum + (next2 - next1 + 1)) rest []
    countTail accum ints@((next1, next2) : restInts) (nextBeaconX : restBeacons)
      | nextBeaconX < next1 = countTail accum ints restBeacons
      | nextBeaconX > next2 = countTail (accum + (next2 - next1)) restInts restBeacons
      | otherwise = countTail (accum - 1) ints restBeacons

findHole :: [Interval] -> Int -> Maybe Int
findHole [] _ = Nothing
findHole [(start, end)] maxCol
  | start > 0 = Just (start - 1)
  | end < maxCol = Just (end + 1)
  | otherwise = Nothing
findHole ((_, end1) : (start2, end2) : rest) maxCol = if end1 + 1 < start2 && (end1 + 1) >= 0 && (end1 + 1) <= maxCol
  then Just (end1 + 1)
  else findHole ((start2, end2) : rest) maxCol

f :: [(Coord, Coord)] -> Int -> Int -> Maybe Int
f pairs maxRow currentRow = let intervals = mergeIntervals . sort $ mapMaybe (excludedCoords currentRow) pairs
                             in findHole intervals maxRow

part1 :: [(Coord, Coord)] -> IO ()
part1 pairs = do
                let selectedRow = 2000000
                let beacons = map snd pairs
                let intervals = mergeIntervals . sort $ mapMaybe (excludedCoords selectedRow) pairs
                let beaconsInRowXs = map fst . nub $ filter (\(_, y) -> y == selectedRow) beacons

                print $ countIntervalsExcludingBeacons intervals beaconsInRowXs

part2 :: [(Coord, Coord)] -> IO ()
part2 pairs = do
          let selectedRow = 4000000
          let lst = zip [0..] $ map (f pairs selectedRow) [0..]

          let f' (_idx, val) = isJust val
          let (x, y) = (\(idx, val) -> (fromJust val, idx)) . head $ filter f' lst

          print (x, y)
          print $ 4000000 * x + y

main :: IO ()
main = do
          let filename = "input.txt"
          contents <- readFile filename
          case runParser parseCoordPairs filename contents of
            Left err -> putStr (errorBundlePretty err)
            Right pairs -> part2 pairs

