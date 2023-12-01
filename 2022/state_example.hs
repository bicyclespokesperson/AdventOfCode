import Control.Monad.State

-- The type of the state is a list of integers
type IntListState = State [Int]

-- A function that appends an integer to the state
addToState :: Int -> IntListState ()
addToState x = do
  state <- get
  put (x : state)

-- A function that removes the first integer from the state
removeFromState :: IntListState ()
removeFromState = do
  state <- get
  case state of
    [] -> return ()
    (x:xs) -> put xs

-- A function that gets the first integer from the state
getFirst :: IntListState (Maybe Int)
getFirst = do
  state <- get
  case state of
    [] -> return Nothing
    (x:xs) -> return (Just x)

{-
-- The main function
main :: IO ()
main = do
  let initialState = [1, 2, 3]
  let (result, finalState) = runState (addToState 4 >> addToState 4 >> removeFromState >> getFirst) initialState
  print result
  print finalState
-}
main :: IO ()
main = do
  let initialState = [1, 2, 3]
  let (result, finalState) =
        runState
          (do addToState 4
              removeFromState
              getFirst)
          initialState
  print finalState
  print result
