import qualified Data.IntMap.Strict as Data.List
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import System.IO
import Text.Read

topThree :: [Int] -> [Int]
topThree ls = take 3 $ L.sortBy (flip compare) ls

readInputData :: T.Text -> IO [Int]
readInputData filename = do
  handle <- openFile (T.unpack filename) ReadMode
  contents <- hGetContents handle
  let notListOfEmpty x = x /= [""]
  let foodsPerElf =
        filter notListOfEmpty $
        L.groupBy (\x y -> null x == null y) $ Prelude.lines contents
  return $ map (sum . M.mapMaybe (readMaybe :: String -> Maybe Int)) foodsPerElf

main :: IO ()
main = do
  x <- readInputData $ T.pack "./input.txt"
  print $ sum $ topThree x
