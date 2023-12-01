import Data.Char (isLower, isUpper, ord, toLower, toUpper)
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

readInputData :: String -> IO [T.Text]
readInputData filename = T.lines <$> Data.Text.IO.readFile filename

findMatchingChar :: (T.Text, T.Text) -> Maybe Char
findMatchingChar (a, b) =
  let s = S.fromList $ T.unpack a
   in L.find (`S.member` s) (T.unpack b)

findCommonChar :: [T.Text] -> Char
findCommonChar (x:xs) =
  let sets = map (S.fromList . T.unpack) xs
   in head $ filter (\val -> all (S.member val) sets) (T.unpack x)

priority :: Char -> Int
priority c
  | isLower c = 1 + (ord c - ord 'a')
  | isUpper c = 26 + priority (toLower c)

main :: IO ()
main = do
  xs <- chunksOf 3 <$> readInputData "input.txt"
  let priorities = map (priority . findCommonChar) xs
  print $ sum priorities
