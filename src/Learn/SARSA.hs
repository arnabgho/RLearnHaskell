module SARSA where

import Table
import qualified Helpers as Helper 
import qualified Data.Map as Map
import Debug.Trace 

updateQ :: (Show state, Show action, Ord state, Ord action) => Double -> state -> action -> state -> action -> Table state action -> Table state action
updateQ reward state action newState newAction q = Table { table = Map.insert (state, action) newQVal (table q)}
						 where thisQ = {- trace ("State: " ++ show(state) ++ "\nAction: " ++ show(action) ) -} table q Map.! (state, action)
						       nextQ = {-trace ("State: " ++ show(newState) ++ "\nAction: " ++ show(newAction) ) -} table q Map.! (newState, newAction) 
						       newQVal = thisQ + Helper.learningRate * ( reward + Helper.discountFactor * nextQ - thisQ ) 