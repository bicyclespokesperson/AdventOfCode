import Data.Foldable (toList)
import qualified Data.Sequence as S
import qualified Data.Set as Set

firstN :: Int -> String -> (S.Seq Char, [Char])
firstN n s = (S.fromList (take n s), drop n s)

unique :: S.Seq Char -> Bool
unique seq =
  let set = Set.fromList $ toList seq
   in length seq == length set

firstUniqueSubstr :: S.Seq Char -> String -> Int -> Int
firstUniqueSubstr cur (hd:rs) idx
  | unique cur = idx
  | otherwise =
    case S.viewl cur of
      S.EmptyL -> -1
      x S.:< xs -> firstUniqueSubstr (xs S.|> hd) rs (idx + 1)
firstUniqueSubstr _ [] _ = -1

main :: IO ()
main = do
  let n = 14
  contents <- readFile "input.txt"
  let (start, rest) = firstN n contents
  let idx = firstUniqueSubstr start rest n
  print idx
