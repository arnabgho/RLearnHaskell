module QLearn  
( getQ,
  giveFeedback,
  initializeState
) where 

import Data.Map (Map)
import qualified Data.Map as Map

-- fe for feedback Fractional
-- st for state Int
-- ac for action Int
-- qval for value of q Fractional

inf=1e9
negInf= -inf
learningRate=0.1
discountFactor=0.1
eps=1e-5

--q= Map.empty

getQ::Int->Int->Double
--getQ::(Integral a,Double b)=>a-> a ->b
getQ state action=q Map.! (state,action)

greater::(Ord a)=>a->a->a
greater x y = if x>y then x else y

getMaxActionVal::Int-> [Int]->Double
--getMaxActionVal:: (Integral a,Double b)=>a-> [a]->b
getMaxActionVal newState actions=let x=map (getQ newState) actions
									in foldl greater negInf x  



checkZero::Int->[Int]->Int
checkZero state [] = -1
checkZero state (action:rest) = if abs(q Map.! (state,action) )<eps then action
								else checkZero state rest									


findAction::Int->[Int]->Double->Int
findAction state [] val= -1
findAction state (action:rest) val=if abs(q Map.! action - val)<eps then action
									else findAction state rest val		


giveFeedback::Double->Int->[Int]->Int->Int->Double
giveFeedback feedback newState actions lastState lastAction=let{
  	maxim=getMaxActionVal newState actions;
    lastQ=q Map.! (lastState,lastAction);
	newQ=lastQ+learningRate*(feedback + discountFactor* maxim - lastQ);
	res=Map.insert (lastState,lastAction) newQ q; 	
	} in newQ														


getAction::Int->[Int]->Int
getAction state actions=if x /= -1 then x else y 
	where{ 
		  x=checkZero state actions; 
		  z=map (getQ state) actions;
		  yval=foldl greater negInf z;
		  y=findAction state actions yval;
		}  

initializeState::Int->[Int]->Int
initializeState state actions=let{
		numActions=length actions;
		states=replicate numActions state;
		keys=zip states actions;
		q= foldl (\map k -> Map.insert k 0 map) q keys;	
	} in state
