module QLearn  
( getQ,
  giveFeedback,
  initializeState,
  Table
) where 

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
eps=1e-5

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
checkZero state (action:rest) q= if abs(q Map.! (state,action) )<eps then action
								else checkZero state rest q									


findAction::Int->[Int]->Double->Table->Int
findAction state [] val q= -1
findAction state (action:rest) val q=if abs(q Map.! (state,action) - val)<eps then action
									else findAction state rest val q	


giveFeedback::Double->Int->[Int]->Int->Int->Table->Table
giveFeedback feedback newState actions lastState lastAction q=let{
  	maxim=getMaxActionVal newState actions q;
    lastQVal=q Map.! (lastState,lastAction);
	newQVal=lastQVal+learningRate*(feedback + discountFactor* maxim - lastQVal);
	newQ=Map.insert (lastState,lastAction) newQVal q; 	
	} in newQ														


getAction::Int->[Int]->Table->Int
getAction state actions q=if x /= -1 then x else y 
	where{ 
		  x=checkZero state actions q; 
		  z=map (getQ state q) actions;
		  yval=foldl greater negInf z;
		  y=findAction state actions yval q;
		}  

initializeState::Int->[Int]->Table->Table
initializeState state actions q=let{
		numActions=length actions;
		states=replicate numActions state;
		keys=zip states actions;
		newQ= foldl (\map k -> Map.insert k 0 map) q keys;	
	} in newQ
