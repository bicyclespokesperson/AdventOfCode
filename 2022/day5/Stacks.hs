import Control.Monad.State
import qualified Data.Char as C
import Data.Foldable (toList)
import qualified Data.Function as F
import Data.Sequence (fromList, mapWithIndex)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Stack = T.Text

data Move =
  Move
    { count :: Int
    , src :: Int
    , dest :: Int
    }

startsWithDigit :: T.Text -> Bool
startsWithDigit s
  | T.null s = False
  | otherwise = C.isDigit $ T.head s

readInputData :: T.Text -> IO ([Stack], [Move])
readInputData filename = do
  contents <- T.lines <$> TIO.readFile (T.unpack filename)
  let (header, moves) = break (T.all C.isSpace) contents
      stacks =
        map (T.reverse . T.tail) .
        filter startsWithDigit . map (T.reverse . T.strip) $
        T.transpose header
      toInts =
        map (\y -> read (T.unpack y) :: Int) . filter startsWithDigit . T.words
      movesList =
        map ((\[c, s, d] -> Move c (s - 1) (d - 1)) . toInts) (tail moves)
   in return (stacks, movesList)

makeMoveOneByOne :: Move -> [Stack] -> [Stack]
makeMoveOneByOne m s =
  let f = makeMoveHelper m
   in iterate f s !! count m

makeMoveHelper :: Move -> [Stack] -> [Stack]
makeMoveHelper m s =
  let rule index st
        | index == src m = T.tail st
        | index == dest m = T.cons (T.head (s !! src m)) st
        | otherwise = st
   in toList $ mapWithIndex rule $ fromList s

makeMoveGrouped :: Move -> [Stack] -> [Stack]
makeMoveGrouped m s =
  let rule index st
        | index == src m = T.drop (count m) st
        | index == dest m = T.take (count m) (s !! src m) <> st
        | otherwise = st
   in toList $ mapWithIndex rule $ fromList s

main :: IO ()
main = do
  (stacks, moves) <- readInputData (T.pack "input.txt")
  let moveFns = map makeMoveGrouped moves
  let endState = foldl (\startState f -> f startState) stacks moveFns
  let result = map T.head $ filter (not . T.null) endState
  print result
