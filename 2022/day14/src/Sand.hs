module Main where

--See https://github.com/clatisus/advent-of-code-y2022/blob/master/src/Day13.hs for inspiration

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Set as S
import qualified Text.Megaparsec.Char.Lexer as LL (decimal)

type Parser = Parsec Void String

type Cave = S.Set (Int, Int)

entryPoint :: (Int, Int)
entryPoint = (500, 0)

readCoordinatePair :: Parser (Int, Int)
readCoordinatePair = do
                    x <- LL.decimal
                    _ <- string ","
                    y <- LL.decimal
                    return (x, y)

readRocks :: Parser [[(Int, Int)]]
readRocks = readRock `sepBy` newline
              where readRock = readCoordinatePair `sepBy` string " -> "

addRock :: Cave -> [(Int, Int)] -> Cave
addRock cave [] = cave
addRock cave [_] = cave
addRock cave ((x1, y1):(x2, y2):xs) = let indicesToSet = if x1 == x2 then [(x1, y) | y <- [(min y1 y2)..(max y1 y2)]] else [(x, y1) | x <- [(min x1 x2)..(max x1 x2)]]
                                          newCave = foldl (flip S.insert) cave indicesToSet
                                       in addRock newCave ((x2, y2):xs)

dropSand :: Cave -> Int -> (Int, Int) -> (Cave, Bool)
dropSand cave maxY (x, y)
  | y > maxY = (cave, False)
  | not (S.member (x, y+1) cave) = dropSand cave maxY (x, y+1)
  | not (S.member (x-1, y+1) cave) = dropSand cave maxY (x-1, y+1)
  | not (S.member (x+1, y+1) cave) = dropSand cave maxY (x+1, y+1)
  | otherwise = (S.insert (x, y) cave, True)

dropSand' :: Cave -> Int -> (Int, Int) -> (Cave, Bool)
dropSand' cave maxY (x, y)
  | y >= maxY = (S.insert (x, y) cave, True)
  | not (S.member (x, y+1) cave) = dropSand' cave maxY (x, y+1)
  | not (S.member (x-1, y+1) cave) = dropSand' cave maxY (x-1, y+1)
  | not (S.member (x+1, y+1) cave) = dropSand' cave maxY (x+1, y+1)
  | otherwise = (S.insert (x, y) cave, (x, y) /= entryPoint)

part1 :: IO ()
part1 = do
  contents <- readFile "input.txt"
  case runParser readRocks "filename_for_error_message.txt" contents of
    Left err -> putStr (errorBundlePretty err)
    Right rocks -> do
                      let cave = foldl addRock S.empty rocks
                      let maxY = S.foldl' (\a b -> max a (snd b)) 0 cave

                      let f cave' startPos = let (cave'', landed) = dropSand cave' maxY startPos 
                                             in if landed then f cave'' startPos else cave'
                      
                      let finalCave = f cave entryPoint

                      print $ S.size finalCave - S.size cave

part2 :: IO ()
part2 = do
  contents <- readFile "input.txt"
  case runParser readRocks "filename_for_error_message.txt" contents of
    Left err -> putStr (errorBundlePretty err)
    Right rocks -> do
                      let cave = foldl addRock S.empty rocks
                      let maxY = S.foldl' (\a b -> max a (snd b)) 0 cave + 1

                      let f cave' startPos = let (cave'', landed) = dropSand' cave' maxY startPos 
                                             in if landed then f cave'' startPos else cave'
                      
                      let finalCave = f cave entryPoint

                      print $ S.size finalCave - S.size cave + 1

main :: IO ()
main = part2
