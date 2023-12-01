import Data.List (splitAt, intercalate)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

parseLine :: String -> [Int -> Int]
parseLine "noop" = [id]
parseLine addX = [id, (+) (read x :: Int)]
  where
    [_, x] = words addX

readInputData :: String -> IO [Int -> Int]
readInputData filename = concatMap parseLine . lines <$> readFile filename

analyzePart1 :: [Int] -> Int
analyzePart1 xs =
  sum . map (uncurry (*)) $
  filter (\(i, x) -> (i - 20) `mod` 40 == 0) $ zip [1 ..] xs

part1 :: IO ()
part1 = do
  fs <- readInputData "input.txt"
  let res = scanl (\v f -> f v) 1 fs
  print $ analyzePart1 res

renderGrid :: [(Int, Int)] -> IO ()
renderGrid xs = putStrLn $ intercalate "\n" grid
                 where grid = splitEvery 40 $ map (\(i, b) -> if abs (i - b) <= 1 then '#' else '.') xs
part2 :: IO ()
part2 = do
  fs <- readInputData "input.txt"
  let res = scanl (\v f -> f v) 1 fs
  let indices = concat $ replicate 6 [0..39]
  renderGrid $ zip indices res

main :: IO ()
main = part1