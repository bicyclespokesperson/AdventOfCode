module Main where

import Data.Char (isDigit)

import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Data.Text.IO as TIO
import Data.Maybe (fromJust)

getFirstLastDigitSum :: T.Text -> Int
getFirstLastDigitSum s = let allDigits = T.filter isDigit s in
                          fromJust $ readMaybe [T.head allDigits, T.last allDigits] :: Int

main :: IO ()
main = do
          let filename = "input.txt"
          contents <- T.lines <$> TIO.readFile filename
          print . sum $ map getFirstLastDigitSum contents



