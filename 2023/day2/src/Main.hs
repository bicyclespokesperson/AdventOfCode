{-# LANGUAGE RecordWildCards   #-}

module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

import Data.List (foldl')
import qualified Data.Map as M
import Data.Void (Void)
import Data.Maybe (fromJust)

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

condenseGames :: [M.Map String Int] -> M.Map String Int
condenseGames = foldl' combine M.empty
  where
    combine = M.foldlWithKey' (\mAcc k v -> M.insertWith max k v mAcc)

condense :: Game -> (Int, Round)
condense Game { gameId, rounds } = (gameId, condenseGames rounds)

enoughCubes :: Round -> Bool
enoughCubes r = fromJust (M.lookup "red" r) <= 12 
                  && fromJust (M.lookup "green" r) <= 13 
                  && fromJust (M.lookup "blue" r) <= 14

power :: Round -> Int
power = M.foldl' (*) 1

part1 :: IO ()
part1 = do
          contents <- readFile "input.txt"
          case runParser (many parseGame) "filename_for_error_message.txt" contents of
            Left e -> putStr (errorBundlePretty e)
            Right games -> do 
              let condensed = map condense games
              print . sum . map fst $ filter (enoughCubes . snd) condensed

part2 :: IO ()
part2 = do
          contents <- readFile "input.txt"
          case runParser (many parseGame) "filename_for_error_message.txt" contents of
            Left e -> putStr (errorBundlePretty e)
            Right games -> do 
              let condensed = map condense games
              print . sum . map (power . snd) $ condensed

main :: IO ()
main = part2
