module QLearn where

import qualified Helpers as Helper
import qualified Data.Map as Map
import Table

getQ :: (Ord state, Ord action) => state -> Table state action-> action -> Double
getQ state q action = table q Map.! (state, action)

greater :: (Ord a) => a -> a -> a
greater x y = if x > y then x else y

getMaxActionVal :: (Ord state, Ord action) => state -> [action] -> Table state action -> Double
--getMaxActionVal:: (Integral a,Double b)=>a-> [a]->b
getMaxActionVal newState actions q = let x = map (getQ newState q) actions in foldl greater Helper.negInf x  

updateQ :: (Show state, Show action, Ord state, Ord action) => Double -> state -> [action] -> state -> action -> Table state action -> Table state action
updateQ feedback newState actions lastState lastAction q = newQ
  where
  maxim = getMaxActionVal newState actions q
  lastQVal = table q Map.! (lastState, lastAction)
  newQVal = lastQVal + Helper.learningRate * (feedback + Helper.discountFactor * maxim - lastQVal)
  newQ = Table {table = Map.insert (lastState,lastAction) newQVal (table q)}