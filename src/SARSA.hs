module SARSA where

import qualified Data.Map as Map
import Debug.Trace 

type Table = Map.Map (Integer, Integer) Double

inf = 1e9
negInf = -inf
learningRate = 0.1
discountFactor = 0.1
eps = 1e-2

initializeState :: Integer -> [Integer] -> Table -> Table
initializeState s [] tab = tab
initializeState s (a:as) tab = Map.insert (s, a) 0 $ initializeState s as tab

initializeStates :: [Integer] -> [Integer] -> Table -> Table
initializeStates [] as tab = tab
initializeStates (s:rs) as tab = initializeStates rs as $ initializeState s as tab
                       
-- TODO epsilon greedy => random
-- TODO maybe type
getAction :: Integer -> [Integer] -> Table -> Integer
getAction s (act:[]) q = trace ("The last act " ++ show(act)) act
getAction s (act:acts) q = bestAction 
			 where thisActionVal = trace ("State: " ++ show(s) ++ "\nAction: " ++ show(act) ) q Map.! (s, act)
                               remActionBest = trace ("with " ++ show(acts) ++ " renaining actions\n") getAction s acts q
                               remVal = q Map.! (s, remActionBest)
                               bestAction = if thisActionVal > remVal then act else remActionBest


updateQ :: Double -> Integer -> Integer -> Integer -> Integer -> Table -> Table
updateQ reward state action newState newAction q = Map.insert (state, action) newQVal q
						 where thisQ = trace ("State: " ++ show(state) ++ "\nAction: " ++ show(action) ) q Map.! (state, action)
						       nextQ = trace ("State: " ++ show(newState) ++ "\nAction: " ++ show(newAction) ) q Map.! (newState, newAction) 
						       newQVal = thisQ + learningRate * ( reward + discountFactor * nextQ - thisQ )

