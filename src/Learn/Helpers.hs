module Helpers where

import qualified Data.Map as Map
import System.Random
import qualified Control.Monad.Reader as Reader

inf = 1e9
negInf = -inf
learningRate = 0.1
discountFactor = 0.1
epsilon = 1e-2

data Table state action = Table {
  table :: Map.Map (state , action ) Double
}

-- getTable :: Table state action -> Reader (Table state action)  

initializeState :: (Ord state, Ord action) => state -> [action] -> Table state action -> Table state action
initializeState s [] tab = tab
initializeState s (a:as) tab = Table {table = Map.insert (s, a) 0 (table $ initializeState s as tab)}

initializeStates :: (Ord state, Ord action) => [state] -> [action] -> Table state action -> Table state action
initializeStates [] as tab = tab
initializeStates (s:rs) as tab = initializeStates rs as $ initializeState s as tab
                       
getRandomAction :: [action] -> StdGen -> action
getRandomAction actions g = randAction 
  where
    numActions = length actions 
    (randomActionIndex,g') = randomR (1,numActions) g :: (Int,StdGen)
    randAction = actions !! (randomActionIndex-1)

getBestAction :: (Ord state, Ord action) => state -> [action] -> Table state action -> action
getBestAction s (act:[]) q = act
getBestAction s (act:acts) q = bestAction
  where
    thisActionVal =  (table q) Map.! (s, act)
    remActionBest = getBestAction s acts q
    remVal = (table q) Map.! (s, remActionBest)
    bestAction = if thisActionVal > remVal then act else remActionBest

getAction:: (Ord state, Ord action) => state -> [action] -> Table state action -> StdGen -> (action,StdGen)
getAction s acts q g = if prob < epsilon then (randAction, g') else (bestAction, g') 
  where	(prob,g') = randomR (0,1.00) g :: (Double,StdGen) 
	randAction = getRandomAction acts g
	bestAction = getBestAction s acts q

