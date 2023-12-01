module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char (ord)
import qualified Data.Array as A
import Data.Void (Void)
import qualified Data.Set as S
import Control.Monad (guard)
import Data.Maybe (fromJust)

type Coord2 = (Int, Int)
type Grid2D a = A.Array Coord2 a
type Parser = Parsec Void String

charToHeight :: Char -> Int
charToHeight 'S' = 100
charToHeight 'E' = charToHeight 'z' + 1
charToHeight c = ord c - ord 'a'

parse2DDigitArray :: Parser (Grid2D Int)
parse2DDigitArray = digitsToArray <$> sepEndBy1 parseDigitLine eol

digitsToArray :: [[Int]] -> Grid2D Int
digitsToArray inputs = A.listArray ((0, 0), (length inputs - 1, length (head inputs) - 1)) (concat inputs)

-- (fmap charToHeight) :: [Char] -> [Int], and that function is applied to the value in the parser's monadic context. Could use map instead of fmap
parseDigitLine :: Parser [Int]
parseDigitLine = fmap charToHeight <$> some letterChar

neighbors :: Grid2D Int -> Coord2 -> [Coord2]
neighbors grid (x, y) = do 
                            let ((xmin, ymin), (xmax, ymax)) = A.bounds grid
                            (x', y') <- [(x+1, y), (x-1,y), (x, y+1), (x, y-1)]
                            _ <- guard (x' `elem` [xmin..xmax] && y' `elem` [ymin..ymax] && ((grid A.! (x', y')) - (grid A.! (x, y))) <= 1)
                            return (x', y')

neighbors' :: Grid2D Int -> Coord2 -> [Coord2]
neighbors' grid (x, y) = do 
                            let ((xmin, ymin), (xmax, ymax)) = A.bounds grid
                            (x', y') <- [(x+1, y), (x-1,y), (x, y+1), (x, y-1)]
                            _ <- guard (x' `elem` [xmin..xmax] && y' `elem` [ymin..ymax] && ((grid A.! (x, y)) - (grid A.! (x', y'))) <= 1)
                            return (x', y')

singleStep :: Grid2D Int -> S.Set Coord2 -> S.Set Coord2
singleStep grid st =  let neighbors'' = neighbors grid
                       in S.foldl' (\a b -> S.union a (S.fromList $ neighbors'' b)) st st

singleStep' :: Grid2D Int -> S.Set Coord2 -> S.Set Coord2
singleStep' grid st =  let neighbors'' = neighbors' grid
                    in S.foldl' (\a b -> S.union a (S.fromList $ neighbors'' b)) st st

find :: Grid2D Int -> Int -> Maybe Coord2
find grid val = case filter (\(_, e) -> val == e) (A.assocs grid) of
                  ((i, _):_) -> Just i
                  [] -> Nothing

findAll :: Grid2D Int -> Int -> [Coord2]
findAll grid val = map fst $ filter (\(_, e) -> val == e) (A.assocs grid)

fewestSteps :: Grid2D Int -> Coord2 -> Coord2 -> Int
fewestSteps grid start end = let g = singleStep grid
                                 vals = iterate g $ S.fromList [start]
                              in length $ takeWhile (not . S.member end) vals

fewestSteps' :: Grid2D Int -> Coord2 -> Int -> Int
fewestSteps' grid start endVal = let g = singleStep' grid
                                     vals = iterate g $ S.fromList [start]
                                     f val s =  S.member val $ S.map (grid A.!) s
                                  in length $ takeWhile (not . f endVal) vals

part1 :: IO ()
part1 = do
  contents <- readFile "input.txt"
  case runParser parse2DDigitArray "filename_for_error_message.txt" contents of
      Left e -> putStr (errorBundlePretty e)
      Right g -> do
                      let end = fromJust $ find g $ charToHeight 'z' + 1
                      let start = fromJust $ find g 100
                      print start
                      print end
                      print $ fewestSteps g start end

part2 :: IO ()
part2 = do
  contents <- readFile "input.txt"
  case runParser parse2DDigitArray "filename_for_error_message.txt" contents of
      Left e -> putStr (errorBundlePretty e)
      Right g -> do
                      let start = fromJust $ find g $ charToHeight 'z' + 1
                      let endVal = charToHeight 'a'
                      print $ fewestSteps' g start endVal

main :: IO ()
main = part2
