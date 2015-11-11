import qualified SARSA_eps as SARSA
import Data.Array
import Debug.Trace
import qualified Data.Map as Map
import System.Random

type Pair = (Int, Int)
type State = (Pair, Pair, Pair)
data Action = U|D|L|R deriving (Show, Eq)

actionToInt :: Action -> Int
actionToInt x = case x of U -> 0
                          D -> 1
                          L -> 2
                          _ -> 3

intToAction :: Int -> Action
intToAction x = case x of 0 -> U
                          1 -> D
                          2 -> L
                          _ -> R

blankRow = array (1,5) [ (i, False) | i <- [1..5] ]
boardAllF = array (1,5) [ (i, blankRow) | i <- [1..5] ]  

{-
 - Takes a list of index pairs and sets those to true in passed array
-}

makeTrue [] board = board
makeTrue ((i,j):rest) board = makeTrue rest modifiedBoard 
  where
    modifiedRow = board!i // [(j,True)]
    modifiedBoard = board // [(i, modifiedRow)]
--Force strict
fBoard = makeTrue [(2,3), (3,3), (4,3)] $! (boardAllF) 
(1, boardDim) = bounds fBoard
boardDSq = boardDim * boardDim

coordToInt :: Pair -> Int
coordToInt (x, y) = (x - 1) * boardDim + y - 1

stateToInt :: State -> Int
stateToInt (mousePos, cheesePos, catPos) = mouseVal * boardDSq * boardDSq + cheeseVal * boardDSq + catVal
  where mouseVal = coordToInt mousePos
        cheeseVal = coordToInt cheesePos
        catVal = coordToInt catPos

valToState :: Int -> Pair                                          
valToState v = (v `div` boardDim + 1, v `mod` boardDim + 1)
                                            
intToState :: Int -> ( Pair, Pair, Pair )
intToState x = ( valToState mouseVal, valToState cheeseVal , valToState catVal )
  where catVal = x `mod` boardDSq
        remVal = (x - catVal) `div` boardDSq
        cheeseVal = remVal `mod` boardDSq
        mouseVal = (remVal - cheeseVal) `div` boardDSq

isPosOccupied :: Pair -> Bool
isPosOccupied (x, y) = fBoard!x!y

isOnBoard :: Pair -> Bool
isOnBoard (x, y) = x > 0 && x <= boardDim && y > 0 && y <= boardDim

isPosFeasible :: Pair -> Bool
isPosFeasible (x, y) = isOnBoard (x, y) && not ( isPosOccupied (x, y) )
                     
getCatPos :: State -> Pair
getCatPos s = pos
  where (mousePos, cheezePos, catPos) = s
        acts = possActionsCat s
        dist (a, b) (c,d) = (a - c) * ( a - c) + (b - d) * (b - d)
        minDistFrom m c1 c2 = if dist m c1 < dist m c2 then c1 else c2 
        pos = foldl (minDistFrom mousePos) catPos (map (makeMove catPos) acts)

makeMove :: Pair -> Action -> Pair
makeMove (xM, yM) act = case act of U -> (xM-1, yM)
                                    D -> (xM+1, yM)
                                    L -> (xM, yM-1)
                                    _ -> (xM, yM+1)

applyAction :: State -> Action -> State
applyAction s action = newState
  where (mousePos, cheesePos, catPos) = s
        newMousePos = makeMove mousePos action
        newCatPos = getCatPos s
        newState = (newMousePos, cheesePos, newCatPos)

possActionsMouse :: State -> [Action]
possActionsMouse s = possActions
  where (mouseState, _, _) = s
        possActions = filter (isPosFeasible . makeMove mouseState) [U, D, L, R]

possActionsCat :: State -> [Action]
possActionsCat s = possActions
  where (_, _, catState) = s
        possActions = filter (isPosFeasible . makeMove catState) [U, D, L, R]

nextStateIntMouse :: Int -> Action ->  Pair
nextStateIntMouse curIntState = makeMove mouseState
  where (mouseState, _, _) = intToState curIntState

isTermState :: Int -> State -> Bool
isTermState iter (mouseState, cheeseState, catState) = iter == 0 || mouseState == cheeseState || catState == mouseState

reward :: State -> Double
reward (mousePos, cheesePos, catPos)
  | mousePos == cheesePos = 50
  | catPos == mousePos = -100
  | otherwise = 0

maxIter = 100
randomSeed = mkStdGen 42
          
initMap = SARSA.initializeStates [1..(boardDSq * boardDSq * boardDSq)] [0..3] Map.empty
initState = ( (2,2) , (1,5) , (2,5) )

learnGame :: State -> SARSA.Table -> Int -> StdGen -> SARSA.Table
learnGame s q iterations seed = learnGame s q' (iterations - 1) newSeed
  where q' = runEpisode s q seed
        (_, newSeed) = randomR (1, 1000000) seed :: (Int, StdGen)
             
runEpisode :: State -> SARSA.Table -> StdGen -> SARSA.Table
runEpisode s q seed = runStep a (stateToInt s) maxIter q g
  where acts = map actionToInt (possActionsMouse s)
        (a, g) =  trace ("Acts are: " ++ show acts )  SARSA.getAction (stateToInt s) acts q seed


runStep ::  Int -> Int -> Int -> SARSA.Table -> StdGen -> SARSA.Table
runStep a s iterL q g = trace ("State" ++ show (intToState s))  $ if isTermState iterL $ intToState s
                                                                  then q
                                                                  else runStep a' (stateToInt s') (iterL-1) q' g'
  where
    s' = applyAction (intToState s) (intToAction a) -- TODO update Cat's position also
    r = reward s' 
    acts = map actionToInt (possActionsMouse s')
    (a', g') = {- trace ("Acts are: " ++ show acts) -} SARSA.getAction (stateToInt s') acts q g
    q' = {- trace("Selected action " ++ show a') -} SARSA.updateQ r s a (stateToInt s') a' q
{-
 Test run :
let y = runEpisode initState initMap
y Map.! ((stateToInt ((5,2),(1,5), (5,4))) , 3)
-}

-- x = runStep 2 (stateToInt initState) 100 initMap randomSeed
x = runEpisode initState initMap randomSeed
-- x = learnGame initState initMap 10 randomSeed 

main  = print $ x Map.! (1,1)
