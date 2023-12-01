{-
f :: Float -> Float
f x = x

g :: Float -> Float
g x = x

f' :: Float -> (Float, String)
f' x = (x, "f was called")

g' :: Float -> (Float, String)
g' x = (x, "g was called")

--bind f' :: (Float,String) -> (Float,String)

-- usually written (x, st) >>= f 
bind :: (Float -> (Float,String)) -> ((Float,String) -> (Float,String))
bind f' (x, st) = let (a, b) = f' x in (a, st ++ b)
--bind f' = \(x, st) -> let (a, b) = f' x in (a, st ++ b)

unit :: Float -> (Float, String)
unit x = (x, "")
-}
------------------------------------------------------------------------------------
{-
fmap :: Functor f => (a -> b) -> f a -> f b

class Monad m where
  (>>=)  :: m a -> (  a -> m b) -> m b
  (>>)   :: m a ->  m b         -> m b
  return ::   a                 -> m a


instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = writer (a, mempty)
    m >>= k  = WriterT $ do
        (a, w)  <- runWriterT m
        (b, w') <- runWriterT (k a)
        return (b, w `mappend` w')

-}
----------------------------------------------------------------------------------
import Control.Monad.State

-- State has two parameters. The second one can be () if only one is needed
-- Define the initial state
initialState :: State [String] Int
initialState = do
  modify (++ ["initial"])
  gets length

-- Define some functions to be called in the list
fn1 :: State [String] Int
fn1 = do
  modify (++ ["fn1"])
  length <$> get

fn2 :: State [String] Int
fn2 = do
  modify (++ ["fn2"])
  gets length -- Same as length <$> get

-- Define the list of functions
moveFns :: [State [String] Int]
moveFns = [fn1, fn2]

-- Use `foldl` to call all of the functions in the list
main :: IO ()
main = do
  let (result, finalState) = runState (foldl (>>) initialState moveFns) []
  print $ "Result: " ++ show result
  print finalState
