module Main where

import Data.Char (isDigit, digitToInt, intToDigit)

import Data.List (minimumBy)
import qualified Data.Text as T
import qualified Data.Text.Internal.Search as T'
import Text.Read (readMaybe)
import qualified Data.Text.IO as TIO
import Data.Maybe (fromJust, isJust)
import Data.Ord (comparing)

getFirstLastDigitSum :: T.Text -> Int
getFirstLastDigitSum s = let allDigits = T.filter isDigit s in
                          fromJust $ readMaybe [T.head allDigits, T.last allDigits] :: Int

-- From: https://stackoverflow.com/questions/48198144/how-do-i-search-for-string-within-a-string-in-haskell
findStringMaybe :: T.Text -> T.Text -> Maybe Int
findStringMaybe x str = case T'.indices x str of
                         (idx:_) -> Just idx
                         _ -> Nothing

findDigitOrString :: T.Text -> Int
findDigitOrString str = let allSpelledDigits = map T.pack ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
                            allNumericDigits = map T.pack ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
                            locs = f str allSpelledDigits ++ f str allNumericDigits in
                            fst $ minimumBy (comparing snd) locs

findDigitOrString' :: T.Text -> Int
findDigitOrString' str = let allSpelledDigits = map (T.reverse . T.pack) ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
                             allNumericDigits = map T.pack ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
                             locs = f str allSpelledDigits ++ f str allNumericDigits in
                             fst $ minimumBy (comparing snd) locs


f :: T.Text -> [T.Text] -> [(Int, Maybe Int)] -- index is second
f str candidates = let allSpelledDigits = zip [1,2..] candidates in
                       filter (\(_, idx) -> isJust idx) $ map (\(digit, name) -> (digit, findStringMaybe name str)) allSpelledDigits

part2LineRunner :: T.Text -> Int
part2LineRunner str = read [intToDigit $ findDigitOrString str, intToDigit . findDigitOrString' $ T.reverse str] :: Int

part1 :: IO ()
part1 = do
          let filename = "input.txt"
          contents <- T.lines <$> TIO.readFile filename
          print . sum $ map getFirstLastDigitSum contents

part2 :: IO ()
part2 = do
          let filename = "input.txt"
          contents <- T.lines <$> TIO.readFile filename
          print . sum $ map part2LineRunner contents

main :: IO ()
main = part2
