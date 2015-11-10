-- import QLearn
import Data.Array

type Pair = (Integer, Integer)
data Action = U|D|L|R

blankRow = array (1,5) [ (i, False) | i <- [1..5] ]
boardAllF = array (1,5) [ (i, blankRow) | i <- [1..5] ]  

{-
 - Takes a list of index pairs and sets those to true in passed array
-}

makeTrue board [] = board
makeTrue board ((i,j):rest) = makeTrue modifiedBoard rest
  where
    modifiedRow = board!i // [(j,True)]
    modifiedBoard = board // [(i, modifiedRow)]

fBoard = makeTrue boardAllF [(2,3), (3,3), (4,3)] 
(1, boardDim) = bounds fBoard
boardDSq = boardDim * boardDim
           
coordToInt :: Pair -> Integer
coordToInt (x, y) = (x - 1) * boardDim + y - 1

stateToInt :: Pair -> Pair -> Pair -> Integer
stateToInt mousePos cheesePos catPos = mouseVal * boardDSq + cheeseVal * boardDSq + catVal
                                       where mouseVal = coordToInt mousePos
                                             cheeseVal = coordToInt cheesePos
                                             catVal = coordToInt catPos

valToState :: Integer -> Pair                                          
valToState v = (v `div` boardDim, v `mod` boardDim )
                                             
intToState :: Integer -> ( Pair, Pair, Pair )
intToState x = ( valToState mouseVal, valToState cheeseVal , valToState catVal )
               where catVal = x `mod` boardDSq
                     remVal = (x - catVal) `div` boardDSq
                     cheeseVal = remVal `mod` boardDSq
                     mouseVal = (remVal - cheeseVal) `mod` boardDSq

isPosOccupied :: Pair -> Bool
isPosOccupied (x, y) = fBoard!x!y

isOnBoard :: Pair -> Bool
isOnBoard (x, y) = x > 1 && x < boardDim && y > 1 && y < boardDim

isPosFeasible :: Pair -> Bool
isPosFeasible (x, y) = isOnBoard (x, y) && not ( isPosOccupied (x, y) )
                      
applyAction :: Pair -> Action -> Pair
applyAction (x, y) action = case action of U -> (x-1, y)
                                           D -> (x+1, y)
                                           L -> (x, y-1)
                                           R -> (x, y+1)
                      

possActionsMouse :: Integer -> [Action]
possActionsMouse int = possActions
                       where (mouseState, _, _) = intToState int
                             possActions = filter (isPosFeasible . applyAction mouseState) [U, D, L, R]

possActionsCat :: Integer -> [Action]
possActionsCat int = possActions
                       where (_, _, catState) = intToState int
                             possActions = filter (isPosFeasible . applyAction catState) [U, D, L, R]

nextStateIntMouse :: Integer -> Action ->  Pair
nextStateIntMouse curIntState = applyAction mouseState
  where (mouseState, _, _) = intToState curIntState

