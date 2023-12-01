-- To run, Change Stacks.hs to StacksMonad.hs in the Makefile
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

performMoveMonad :: Move -> State [Stack] ()
performMoveMonad m = do
  s <- get
  let rule index st
        | index == src m = T.drop (count m) st
        | index == dest m = T.take (count m) (s !! src m) <> st
        | otherwise = st
  put (toList $ mapWithIndex rule $ fromList s)

main :: IO ()
main = do
  (stacks, moves) <- readInputData (T.pack "input.txt")
  let moveFns = map performMoveMonad moves
  -- Use runState instead of execState if the result of the computation is needed.
  -- Use foldl instead of foldl1 to show (return ()) as the identity function in the example
  let endState = execState (foldl (>>) (return ()) moveFns) stacks
  let result = map T.head $ filter (not . T.null) endState
  print result
