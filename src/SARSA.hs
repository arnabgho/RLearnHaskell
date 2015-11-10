 module SARSA where

 import qualified Data.Map as Map

 type Table = Map.Map (Int,Int) Double

 inf = 1e9
 negInf = -inf
 learningRate = 0.1
 discountFactor = 0.1
 eps = 1e-2

 initializeState :: Int -> [Int] -> Table -> Table
 initializeState a [] q = q
 initializeState s (a:as) q = Map.insert (s, a) 0 $ initializeState s as q 

 -- TODO epsilon greedy => random
 -- TODO maybe type
 getAction :: Int -> [Int] -> Table -> Int
 getAction s [] q = -1
 getAction s (act:acts) q = bestAction 
				 where thisActionVal = q Map.! (s, act)
                                       remActionBest = getAction s acts q
                                       remVal = q Map.! remActionBest
                                       bestActionVal = if thisActionVal > remVal then act else remActionBest

 updateQ :: Double -> Int -> Int -> Int -> Int -> Table -> Table
 updateQ reward state action newState newAction q = Map.insert (state, action) newQVal q
							 where thisQ = q Map.! (state, action)
							       nextQ = q Map.! (newState, newAction) 
							       newQVal = thisQ + learningRate * ( reward + gamma * nextQ - thisQ )

