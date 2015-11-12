module Learner where

import qualified SARSA
import qualified QLearn
import qualified Helpers
import Table
import System.Random
import qualified Game 
import qualified Data.Map as Map

maxIter = 100
randomSeed = mkStdGen 540

initMap = Helpers.initializeStates [1..625] [0..3] Table {table = Map.empty}
initState = ( (2,2) , (1,5) , (2,5) )

learnGame :: (Show state, Show action, Ord state, Ord action) => Game.Game state action => state -> Table state action -> Integer -> StdGen -> Table state action
learnGame game s q iterations seed = if iterations == 0 then q else learnGame game s q' (iterations - 1) newSeed
  where q' = {- trace ("Ran one episode and now state is " ++ show s) -} runEpisode game s q seed
        (_, newSeed) = randomR (1, 1000) seed :: (Double, StdGen)
             
runEpisode :: (Show state, Show action, Ord state, Ord action) => Game.Game state action=> state -> Table state action -> StdGen -> Table state action
runEpisode game s q seed = runStep game a s maxIter q g
  where acts = {- trace ("Calculated next acts")-}  Game.getPossibleActions game s
        (a, g) =  {- trace ("Acts are: " ++ show acts ) -}  Helpers.getAction s acts q seed


runStep :: (Show state, Show action, Ord state, Ord action) => Game.Game state action => action -> state -> Integer -> Table state action -> StdGen -> Table state action
runStep game a s iterL q g = {- trace ("State" ++ show (intToState s)) -  $-}if Game.isTermState game s || iterL == 0
                                                                  	then q
                                                                  	else runStep game a' s' (iterL-1) q' g'
  where
    s' = Game.nextState game s a
    r = Game.reward game s' 
    acts = Game.getPossibleActions game s'
    (a', g') = {- trace ("Acts are: " ++ show acts) -} Helpers.getAction s' acts q g
    q' = {- trace("Selected action " ++ show a') -} SARSA.updateQ r s a s' a' q
{--}