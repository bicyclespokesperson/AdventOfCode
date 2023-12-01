module Main where

--See https://github.com/clatisus/advent-of-code-y2022/blob/master/src/Day13.hs for inspiration

import Data.Maybe (fromJust)
import Data.List (sort, elemIndex)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as LL (decimal)

data Packet = S Int | L [Packet] deriving (Show, Eq)

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (S v) (S v2) = compare v v2
  compare (L (x:xs)) (L (x2:xs2)) = let res = compare x x2 in if res == EQ then compare xs xs2 else res
  compare (S v) (L ls) = compare (L [S v]) (L ls)
  compare (L ls) (S v) = compare (L ls) (L [S v])
  compare (L []) (L []) = EQ
  compare (L []) _ = LT
  compare _ (L []) = GT

type Parser = Parsec Void String

readPacket :: Parser Packet
readPacket = S <$> LL.decimal <|> L <$> between (char '[') (char ']') (readPacket `sepBy` char ',')

readPacketPair :: Parser (Packet, Packet)
readPacketPair = (,) <$> (readPacket <* newline) <*> (readPacket <* newline)

readPackets :: Parser [(Packet, Packet)]
readPackets = readPacketPair `sepBy1` newline

indicesInCorrectOrder :: [(Packet, Packet)] -> [Int]
indicesInCorrectOrder packets = let f (_, pair) = uncurry (<=) pair
                                 in map fst $ filter f $ zip [1 ..] packets

part1 :: IO ()
part1 = do
  contents <- readFile "input.txt"
  case runParser readPackets "filename_for_error_message.txt" contents of
      Left err -> putStr (errorBundlePretty err)
      Right packets -> do 
        let indices = indicesInCorrectOrder packets
        print $ sum indices


part2 :: IO ()
part2 = do
  contents <- readFile "input.txt"
  case runParser readPackets "filename_for_error_message.txt" contents of
      Left err -> putStr (errorBundlePretty err)
      Right packets -> do 
        let div1 = L [ L [S 2]]
        let div2 = L [ L [S 6]]
        let allPackets = sort $ div1 : div2 : concatMap (\(a, b) -> [a, b]) packets
        --mapM_ print allPackets
        print $ (fromJust (elemIndex div1 allPackets) + 1) * (fromJust (elemIndex div2 allPackets) + 1)

main :: IO ()
main = part2
