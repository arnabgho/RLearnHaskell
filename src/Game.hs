module Game where

data Game state action = Game{
  isTermState :: state -> Bool,
  reward :: state -> (Double, Double),
  nextState :: state -> (action, action) -> state,
  getPossibleActions :: state -> ([action],[action]),
  startState :: state,
  trainer :: state -> (action, action)
  }

data Player = Player1 | Player2

data InteractiveGame state action = InteractiveGame{
  game :: Game state action,
  human :: Player
  }

--playAgainst :: HumanPlayer -> InteractiveGame
