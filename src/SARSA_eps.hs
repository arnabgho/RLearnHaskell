module SARSA where
import System.Random
import qualified Data.Map as Map

type Table = Map.Map (Integer, Integer) Double

inf = 1e9
negInf = -inf
learningRate = 0.1
discountFactor = 0.1
epsilon = 1e-2

initializeState :: Integer -> [Integer] -> Table -> Table
initializeState s [] tab = tab
initializeState s (a:as) tab = Map.insert (s, a) 0 $ initializeState s as tab

initializeStates :: [Integer] -> [Integer] -> Table -> Table
initializeStates [] as tab = tab
initializeStates (s:rs) as tab = initializeStates rs as $ initializeState s as tab
                       

getValAtIndex::[Integer]->Integer->Integer
getValAtIndex [] _ = -1
getValAtIndex (x:xs) 1 = x
getValAtIndex (x:xs) n = getValAtIndex xs n-1

getRandomAction::[Integer]->StdGen->Integer
getRandomAction actions g=randAction 
	where{
			numActions = toInteger $ length actions ;
			(randomActionIndex,g')=randomR (1,numActions) g ::(Integer,StdGen);
			randAction=getValAtIndex actions randomActionIndex;
		}

-- TODO epsilon greedy => random
-- TODO maybe type
getBestAction :: Integer -> [Integer] -> Table -> Integer
getBestAction s [] q = -1
getBestAction s (act:acts) q = bestAction 
			 where thisActionVal = q Map.! (s, act)
                               remActionBest = getBestAction s acts q
                               remVal = q Map.! (s, remActionBest)
                               bestAction = if thisActionVal > remVal then act else remActionBest

getAction::Integer->[Integer]->Table->StdGen->(Integer,StdGen)
getAction state actions q g=if prob<epsilon then (randAction,g') else (bestAction,g') 
	where{ 
			(prob,g')=randomR (0,1.00) g :: (Double,StdGen) ;
			randAction=getRandomAction actions g;
			bestAction=getBestAction state actions q;
		}  

updateQ :: Double -> Integer -> Integer -> Integer -> Integer -> Table -> Table
updateQ reward state action newState newAction q = Map.insert (state, action) newQVal q
						 where thisQ = q Map.! (state, action)
						       nextQ = q Map.! (newState, newAction) 
						       newQVal = thisQ + learningRate * ( reward + discountFactor * nextQ - thisQ )

