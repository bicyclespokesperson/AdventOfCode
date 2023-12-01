{-
import Control.Monad
import Control.Monad.State

type GameState = (Int, Int)

move :: GameState -> [GameState]
move (a, b) = [(a+1,b), (a,b+1), (a-1,b), (a,b-1)]

gameStates :: GameState -> StateT GameState [] GameState
gameStates initialState = StateT $ \s ->
  concatMap (\next -> [(next, gameStates next)]) (move s)

runGameStates :: GameState -> [GameState]
runGameStates initialState =
  evalStateT (gameStates initialState) initialState
  -}

import Control.Monad             (guard, mfilter)
import Control.Monad.Trans.State
import Data.Maybe                (fromJust)
import Control.Monad.Trans.Maybe
import Data.List                 (foldl', delete)

-- From: https://blog.jle.im/entry/unique-sample-drawing-searches-with-list-and-statet
-- C++ Translation: https://bartoszmilewski.com/2015/05/11/using-monads-in-c-to-solve-constraints-1-the-list-monad/
asNumber :: [Int] -> Int
asNumber = foldl' (\t o -> t*10 + o) 0

select :: [a] -> [(a, [a])]
select []     = []
select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]

{-
select' :: StateT [a] [] a
select' = StateT select

f' :: StateT [Int] [] (Int, Int, Int)
f' = do 
    s <- select'
    e <- select'
    n <- select'
    d <- select'
    m <- select'
    o <- select'
    r <- select'
    y <- select'
    guard $ s /= 0 && m /= 0
    let send  = asNumber [s,e,n,d]
        more  = asNumber [m,o,r,e]
        money = asNumber [m,o,n,e,y]
    guard $ send + more == money
    return (send, more, money)

constraintPropogation :: IO ()
constraintPropogation = print . flip evalStateT [0..9] $ f'

-}

f :: StateT [Int] IO Double
f = let f' x = if null x then return (1.0, [3,2]) else return (2.0, [2,3])
     in StateT f'

solver1 = 
    [(s, e, n, d , m, o, r , e, m, o, n, e, y)
    | s <- digit, s > 0,
      e <- digit, n <- digit, d <- digit,
      m <- digit, m > 0,
      o <- digit, r <- digit, y <- digit,
      different [s, e, n, d , m, o, r , y ],
      num [s, e, n, d ] + num [m, o, r , e ]
      == num [m, o, n, e, y ]]
      where digit = [0 .. 9]

different :: (Eq a) => [a] -> Bool
different []     = True
different (x:xs) = x `notElem` xs && different xs

num :: [Int] -> Int
num = foldl ((+) . (* 10)) 0

solver2 :: [(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]
solver2 = do
   let m = 1
   let o = 0
   s <- digit
   guard (s > 7)
   e <- digit
   n <- digit
   d <- digit
   r <- digit
   y <- digit
   guard (different [s, e, n, d , m, o, r, y ])
   guard (num [s, e, n, d] + num [m, o, r, e] == num [m, o, n, e, y])
   return (s, e, n, d, m, o, r, e, m, o, n, e, y)
   where digit = [2 .. 9]

splits :: Eq a => [a] -> [(a, [a])]
splits list = list >>= \x -> return (x , delete x list)

choose :: StateT [Int] [ ] Int
choose = StateT splits

-- data StateT s m a = StateT (s -> m (a, s))

--f'' :: StateT [Int] Maybe Bool
--f'' = StateT $ do let g xs = if null xs then Maybe True else Nothing

mFn :: Int -> Maybe Bool
mFn x = if x > 4 then Just True else Just False

-- The last return type can vary (e.g. solver3 can call this function). The state type ([Int] in this case)
-- and the internal monad type ([] in this case) must match.
testFn :: StateT [Int] [] Bool
testFn = do let m = 1
            let o = 0
            v <- get
            let s = mFn 4 >>= (\b -> return  $ b && o > m && null v)
            return $ fromJust s

solver3 :: StateT [Int] [ ] [Int]
solver3 = do let m = 1
             let o = 0
             s <- choose
             guard (s > 7)
             e <- choose
             d <- choose
             mm <- testFn
             put [1..3]
             y <- choose
             ss <- get
             put $ ss ++ [1]
             n <- choose
             r <- choose
             guard (num [s, e, n, d] + num [m, o, r, e] == num [m, o, n, e, y])
             return [s, e, n, d, m, o, r, y]

main :: IO ()
main = do
    print $ replicate 100 '-'
    print solver1
    print solver2
    print $ evalStateT solver3 [2..9]
