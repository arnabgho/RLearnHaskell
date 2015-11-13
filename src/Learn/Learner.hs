module Learner where

import qualified SARSA
import qualified QLearn
import qualified Helpers
import Table
import System.Random
import qualified Game 
import qualified Data.Map as Map
import Debug.Trace 

maxIter = 100

learnGame :: (Show state, Show action, Ord state, Ord action) => Game.Game state action -> Game.Player -> state -> Table state action -> Integer -> StdGen -> Table state action
learnGame game human s q iterations seed = if iterations == 0 then q else learnGame game human s q' (iterations - 1) newSeed
  where q' = {- trace ("Ran one episode and now state is " ++ show s) -} runEpisode game human s q seed
        (_, newSeed) = randomR (1, 1000) seed :: (Double, StdGen)
             
runEpisode :: (Show state, Show action, Ord state, Ord action) => Game.Game state action -> Game.Player -> state -> Table state action -> StdGen -> Table state action
runEpisode game human s q seed = runStep game human a s maxIter q g
  where acts = case human of 
          Game.Player1 -> {- trace ("Calculated next acts")-} snd $ Game.getPossibleActions game s
          Game.Player2 -> fst $ Game.getPossibleActions game s
        (a, g) =  {- trace ("Acts are: " ++ show acts ) -}  Helpers.getAction s acts q seed


runStep :: (Show state, Show action, Ord state, Ord action) => Game.Game state action -> Game.Player ->  action -> state -> Integer -> Table state action -> StdGen -> Table state action
runStep game human a s iterL q g = {- trace ("State" ++ show (intToState s)) -  $-}if Game.isTermState game s || iterL == 0
                                                                  	then q
                                                                  	else runStep game human a' s' (iterL-1) q' g'
  where
    as = Game.trainer game s
    pa = case human of
           Game.Player1 -> (fst as, a)
           Game.Player2 -> (a, snd as)
    s' = Game.nextState game s pa
    (r, acts) = case human of
         Game.Player1 -> (snd $ Game.reward game s', snd $ Game.getPossibleActions game s')
         Game.Player2 -> (fst $ Game.reward game s', fst $ Game.getPossibleActions game s')
    (a', g') =  {-trace ("Acts are: " ++ show acts)-} Helpers.getAction s' acts q g
    q' =  {-trace("Selected action " ++ show a')-} SARSA.updateQ r s a s' a' q
{--}
