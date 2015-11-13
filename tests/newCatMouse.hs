import Game
import Table
state :: [[Int]]
action :: (Int, Int)

isTermState :: state -> Bool
isTermState s 
reward :: state -> (Double, Double)
nextState :: state -> (action, action) -> state
getPossibleActions :: state -> ([action], [action])
startState :: state

tictactoe :: Game state action
tictactoe = Game 
