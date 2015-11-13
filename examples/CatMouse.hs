import qualified Game
import qualified Learner
import Table
import Helpers
import Data.Array
import qualified Data.Map as Map
import System.Random
import Debug.Trace

type Pair = (Integer, Integer)
type State = (Pair, Pair, Pair) 
data Action = U|D|L|R deriving (Show, Eq, Ord)


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
fBoard = makeTrue [(2,3), (3,3), (4,3)] boardAllF
(1, boardDim) = bounds fBoard
boardDSq = boardDim * boardDim

coordToInteger :: Pair -> Integer
coordToInteger (x, y) = (x - 1) * boardDim + y - 1


valToState :: Integer -> Pair                                          
valToState v = (v `div` boardDim + 1, v `mod` boardDim + 1)

isPosOccupied :: Pair -> Bool
isPosOccupied (x, y) = fBoard!x!y

isOnBoard :: Pair -> Bool
isOnBoard (x, y) = x > 0 && x <= boardDim && y > 0 && y <= boardDim

isPosFeasible :: Pair -> Bool
isPosFeasible (x, y) = isOnBoard (x, y) && not ( isPosOccupied (x, y) )
                     
getCatPos :: State -> Action
getCatPos s = pos
  where (mousePos, cheezePos, catPos) = s
        acts = possActionsCat s
        dist (a, b) (c,d) = (a - c) * (a - c) + (b - d) * (b - d)
        minDistFrom m act1 act2 = if dist m (makeMove catPos act1) < dist m (makeMove catPos act2) then act1 else act2 
        pos = foldl1 (minDistFrom mousePos) acts

getMousePos :: State -> Action
getMousePos _ = U -- why mouse why?

aiForPlayers :: State -> (Action, Action)
aiForPlayers s = (getMousePos s, getCatPos s) 

makeMove :: Pair -> Action -> Pair
makeMove (xM, yM) act = case act of U -> (xM-1, yM)
                                    D -> (xM+1, yM)
                                    L -> (xM, yM-1)
                                    _ -> (xM, yM+1)

applyAction :: State -> (Action, Action) -> State
applyAction s (actionM, actionC) = newState
  where (mousePos, cheesePos, catPos) = s
        newMousePos = makeMove mousePos actionM
        newCatPos =  makeMove catPos actionC
        newState = {- trace ( show actionM ++ show mousePos ++ show actionC ++ show catPos) -} (newMousePos, cheesePos, newCatPos)

possActionsMouse :: State -> [Action]
possActionsMouse s = possActions
  where (mouseState, _, _) = s
        possActions = filter (isPosFeasible . makeMove mouseState) [U, D, L, R]

possActionsCat :: State -> [Action]
possActionsCat s = possActions
  where (_, _, catState) = s
        possActions = filter (isPosFeasible . makeMove catState) [U, D, L, R]

possActions :: State -> ([Action], [Action])
possActions s = (possActionsMouse s, possActionsCat s)

isTermState :: State -> Bool
isTermState (mouseState, cheeseState, catState) = mouseState == cheeseState || catState == mouseState

reward :: State -> (Double, Double)
reward (mousePos, cheesePos, catPos)
  | mousePos == cheesePos = (50, -100)
  | catPos == mousePos = (-100, 50)
  | otherwise = (0, 0)

intToState :: Integer -> ( Pair, Pair, Pair )
intToState x = (valToState mouseVal, valToState cheeseVal , valToState catVal )
  where catVal = x `mod` boardDSq
        remVal = (x - catVal) `div` boardDSq
        cheeseVal = remVal `mod` boardDSq
        mouseVal = (remVal - cheeseVal) `div` boardDSq

--emptyTypedMap = Map.fromList [((initState,U),0)]
initMap = Helpers.initializeStates (map intToState [0..(boardDSq * boardDSq * boardDSq)]) [U, D, L, R] (Table Map.empty)
initState = ( (2,2) , (1,5) , (2,5) )


-- x = runStep 2 (stateToInteger initState) 100 initMap randomSeed
-- x = runEpisode initState initMap randomSeed
-- x = learnGame initState initMap 100 randomSeed 

main  = do 
      gen <- getStdGen
      let game = Game.Game isTermState reward applyAction possActions initState aiForPlayers
          x = Learner.learnGame game Game.Player2 initState initMap 100 gen
      putStrLn (show $  head $ Map.toDescList (table x))

