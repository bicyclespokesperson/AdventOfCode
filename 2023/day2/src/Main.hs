{-# LANGUAGE RecordWildCards   #-}

module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Text.IO as TIO
import Data.Void (Void)

type Parser = Parsec Void String

type Round = M.Map String Int

data Game = Game {
  gameId :: Int,
  rounds :: [Round]
} deriving Show;

parseEntry :: Parser (String, Int)
parseEntry = do
  n <- decimal <* char ' '
  space
  color <- some letterChar
  return (color, n)

parseRound :: Parser Round
parseRound = do 
  e <- parseEntry `sepBy` string ", "
  return $ M.fromList e

parseGame :: Parser Game
parseGame = do
  _ <- string "Game "
  gameId <- decimal <* string ": "
  rounds <- parseRound `sepBy` string "; " <* newline
  return Game {..}

part1 :: IO ()
part1 = do
          contents <- readFile "sample_input.txt"
          case runParser (many parseGame) "filename_for_error_message.txt" contents of
              Left e -> putStr (errorBundlePretty e)
              Right games -> do print games

part2 :: IO ()
part2 = do
          let filename = "input.txt"
          contents <- T.lines <$> TIO.readFile filename
          print $ head contents

main :: IO ()
main = part1
