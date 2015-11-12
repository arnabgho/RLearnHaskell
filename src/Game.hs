module Game
       (
         Game(..),
         InteractiveGame(..)
       )
       where

data Game state action = Game{
  isTermState :: state -> Bool,
  reward :: state -> Double,
  nextState :: state -> action -> state,
  getPossibleActions :: state -> [action],
  startState :: state
  }

data Player = Player1 | Player2

data InteractiveGame state action = InteractiveGame{
  game :: Game,
  aiPlayer :: Player
  }

playAgainst :: HumanPlayer -> InteractiveGame
