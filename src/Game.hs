module Game where
import qualified Control.Monad as Monad
import qualified Helpers
import System.Random
import qualified Table

data Game state action = Game{
  isTermState :: state -> Bool,
  reward :: state -> (Double, Double),
  nextState :: state -> (action, action) -> state,
  getPossibleActions :: state -> ([action],[action]),
  startState :: state,
  trainer :: state -> (action, action)
  }

data Player = Player1 | Player2 deriving (Show)

data InteractiveGame state action = InteractiveGame{
  game :: Game state action,
  human :: Player
  }

--playAgainst :: InteractiveGame -> IO String
-- playAgainst g q = do
--   putStrLn $ "You are player2 "
--   gen <- newStdGen
--   playGame1 gen g startState q
--     -- Player2 -> playGame2 game g
--   putStrLn "Thanks for playing!"
  
playGameInteractive :: (Show action, Show state, Ord action, Ord state) => StdGen -> Game state action -> state -> Table.Table state action -> IO ()
playGameInteractive gen g s q =
  if not ( isTermState g s ) then 
    do
      putStrLn $ "Current state " ++ show s
      let (p1, p2) = getPossibleActions g s 
          (myAct, gn') = Helpers.getAction s p1 q gen
      putStrLn $ "My move: " ++ show myAct
      putStrLn $ "Please choose the index (0.."++ show(length p2 - 1) ++") of your chosen action among " ++ show p2
      playerMove <- getLine
      let r = read playerMove :: Int
          act2 = p2 !! r
      playGameInteractive gn' g (nextState g s (myAct, act2)) q 
  else
    putStrLn $ getOutcome (reward g s)
                  
                  
getOutcome :: (Double, Double) -> String
getOutcome (p1, p2) = if p1 > p2 then "Player 1 wins!"
    else if p1 < p2 then "Player 2 wins!"
         else "Tie!"
