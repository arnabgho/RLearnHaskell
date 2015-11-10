module QLearn  
where 
import System.Random

--randomProbs=randomRs (0, 1.0) (mkStdGen 42)

import Data.Map (Map)
import qualified Data.Map as Map

-- fe for feedback Fractional
-- st for state Int
-- ac for action Int
-- qval for value of q Fractional
type Table = Map.Map (Int,Int) Double

inf=1e9
negInf= -inf
learningRate=0.1
discountFactor=0.1
tolerance=1e-5
epsilon=0.1
--q= Map.empty

getQ::Int->Table->Int->Double
--getQ::(Integral a,Double b)=>a-> a ->b
getQ state q action=q Map.! (state,action)

greater::(Ord a)=>a->a->a
greater x y = if x>y then x else y

getMaxActionVal::Int-> [Int]->Table->Double
--getMaxActionVal:: (Integral a,Double b)=>a-> [a]->b
getMaxActionVal newState actions q=let x=map (getQ  newState q) actions
									in foldl greater negInf x  



checkZero::Int->[Int]->Table->Int
checkZero state [] q= -1
checkZero state (action:rest) q= if abs(q Map.! (state,action) )<tolerance then action
								else checkZero state rest q									


findAction::Int->[Int]->Double->Table->Int
findAction state [] val q= -1
findAction state (action:rest) val q=if abs(q Map.! (state,action) - val)<tolerance then action
									else findAction state rest val q	


giveFeedback::Double->Int->[Int]->Int->Int->Table->Table
giveFeedback feedback newState actions lastState lastAction q=let{
  	maxim=getMaxActionVal newState actions q;
    lastQVal=q Map.! (lastState,lastAction);
	newQVal=lastQVal+learningRate*(feedback + discountFactor* maxim - lastQVal);
	newQ=Map.insert (lastState,lastAction) newQVal q; 	
	} in newQ														

getValAtIndex::[Int]->Int->Int
getValAtIndex [] _ = -1
getValAtIndex (x:xs) 1 = x
getValAtIndex (x:xs) n = getValAtIndex xs n-1

getRandomAction::[Int]->StdGen->Int
getRandomAction actions g=randAction 
	where{
			numActions = length actions;
			(randomActionIndex,g')=randomR (1,numActions) g;
			randAction=getValAtIndex actions randomActionIndex;
		}

getAction::Int->[Int]->Table->StdGen->(Int,StdGen)
getAction state actions q g=if prob<epsilon then (randAction,g') else (bestAction,g') 
	where{ 
			(prob,g')=randomR (0,1.00) g :: (Double,StdGen) ;
			randAction=getRandomAction actions g;
			qValues=map (getQ state q) actions;
			maxQVal=foldl greater negInf qValues;
			bestAction=findAction state actions maxQVal q;
		}  

initializeState::Int->[Int]->Table->Table
initializeState state actions q=let{
		numActions=length actions;
		states=replicate numActions state;
		keys=zip states actions;
		newQ= foldl (\map k -> Map.insert k 0 map) q keys;	
	} in newQ
