import Control.Monad.State
import qualified Data.List as L
import Data.Functor (($>))
import qualified Data.Map as M
import qualified Data.Maybe as MB
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

cdHome = T.pack "$ cd /"

cdUp = T.pack "$ cd .."

cdPrefix = T.pack "$ cd "

lsPrefix = T.pack "$ ls"
 -- The state is a path represented as a list of directories, with the first elem being the lowest level

parseLine ::
     M.Map [T.Text] Integer -> T.Text -> State [T.Text] (M.Map [T.Text] Integer)
parseLine fileSizes cmd
  | cdHome == cmd = put [T.pack "/"] $> fileSizes
  | cdUp == cmd = modify tail $> fileSizes
  | T.isPrefixOf cdPrefix cmd = do
    let dir =
          reverse . T.splitOn (T.pack "/") . MB.fromJust $
          T.stripPrefix cdPrefix cmd
    modify (dir ++)
    return fileSizes
  | T.isPrefixOf lsPrefix cmd = return fileSizes
  | T.null cmd = return fileSizes
  | otherwise = do
    let (sz, name) = T.span (/= ' ') cmd
    case (readMaybe (T.unpack sz) :: Maybe Integer) of
      (Just size) -> do
        curr <- get
        let fullPath = T.strip name : curr
        return $ M.insert fullPath size fileSizes
      Nothing -> return fileSizes -- Ignore directories

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

removeFilenames :: [T.Text] -> Integer -> ([[T.Text]], Integer)
removeFilenames dirs sz =
  (filter (not . null) . map safeTail $ L.tails dirs, sz)

toDirectorySizes ::
     M.Map [T.Text] Integer -> [T.Text] -> Integer -> M.Map [T.Text] Integer
toDirectorySizes m k v =
  let (dirs, sz) = removeFilenames k v
      f m dir = M.insertWith (+) dir sz m
   in foldl f m dirs

part1 :: IO ()
part1 = do
  contents <- T.lines <$> TIO.readFile "input.txt"
  let (fileSizes, _) = runState (foldM parseLine M.empty contents) [T.pack "/"]
  let dirSizes = M.toList $ M.foldlWithKey' toDirectorySizes M.empty fileSizes
  let result = sum . filter (<= 100000) $ map snd dirSizes
  print result

part2 :: IO ()
part2 = do
  contents <- T.lines <$> TIO.readFile "input.txt"
  let (fileSizes, _) = runState (foldM parseLine M.empty contents) [T.pack "/"]
  let dirSizes = M.toList $ M.foldlWithKey' toDirectorySizes M.empty fileSizes
  let totalSpace = 70000000
  let neededSpace = 30000000
  let usedSpace = MB.fromJust $ lookup [T.pack "/"] dirSizes
  let target = neededSpace - (totalSpace - usedSpace)
  let result = minimum . filter (>= target) $ map snd dirSizes
  print result

main :: IO ()
main = part2
