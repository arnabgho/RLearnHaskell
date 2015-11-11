module SARSA_eps where
import System.Random
import qualified Data.Map as Map
import Debug.Trace

type Table = Map.Map (Int, Int) Double

inf = 1e9
negInf = -inf
learningRate = 0.1
discountFactor = 0.1
epsilon = 1e-2

initializeState :: Int -> [Int] -> Table -> Table
initializeState s [] tab = tab
initializeState s (a:as) tab = Map.insert (s, a) 0 $ initializeState s as tab

initializeStates :: [Int] -> [Int] -> Table -> Table
initializeStates [] as tab = tab
initializeStates (s:rs) as tab = initializeStates rs as $ initializeState s as tab
                       
getRandomAction::[Int]->StdGen->Int
getRandomAction actions g=randAction 
	where{
			numActions = length actions ;
			(randomActionIndex,g')=randomR (1,numActions) g ::(Int,StdGen);
			randAction=actions !! (randomActionIndex-1);
		}

-- TODO epsilon greedy => random
-- TODO maybe type
getBestAction :: Int -> [Int] -> Table -> Int
getBestAction s (act:[]) q = act
getBestAction s (act:acts) q = bestAction
  where thisActionVal =  q Map.! (s, act)
        remActionBest = getBestAction s acts q
        remVal = q Map.! (s, remActionBest)
        bestAction = if thisActionVal > remVal then act else remActionBest

getAction::Int->[Int]->Table->StdGen->(Int,StdGen)
getAction state actions q g=if prob<epsilon then (randAction,g') else (bestAction,g') 
  where	(prob,g')=randomR (0,1.00) g :: (Double,StdGen) 
	randAction=getRandomAction actions g
	bestAction=getBestAction state actions q
 

updateQ :: Double -> Int -> Int -> Int -> Int -> Table -> Table
updateQ reward state action newState newAction q = Map.insert (state, action) newQVal q
						 where thisQ = q Map.! (state, action)
						       nextQ = q Map.! (newState, newAction) 
						       newQVal = thisQ + learningRate * ( reward + discountFactor * nextQ - thisQ )

