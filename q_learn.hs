import Data.Map (Map)
import qualified Data.Map as Map

-- fe for feedback 
-- st for state
-- ac for action
-- qval for value of q

inf=1e9
learningRate=0.1
discountFactor=0.1

q= Map.empty

getQ::st->ac->qval
getQ state action=q Map.! (state,action)

greater::(Ord a)=>a->a->a
greater x y = if x>y then x else y

getMaxActionVal::(Ord qval)=> st-> [ac]->qval
getMaxActionVal newState actions=let x=map (getQ newState) actions
									in foldl greater -inf x  

giveFeedback::fe->st->[ac]->st->ac->Bool
giveFeedback feedback newState actions lastState lastAction=let maxim=0
										maxim=getMaxActionVal actions
										lastQ=q Map.!(lastState,lastAction)
										newQ=lastQ+learningRate*(feedback + discountFactor* maxim - lastQ)
										Map.insert (lastState,lastAction) newQ q 
										in True 

checkZero::st->[ac]->ac
checkZero state [] = -1
checkZero state (action:rest) = if q Map.! (state,action) ==0 then action
								else checkZero state rest

findAction::st->[ac]->qval->ac
findAction state [] val=-1
findAction state (action:rest) val=if q Map.! action == val then action
									else findAction state rest val

getAction::st->[ac]->ac
getAction state actions=if x \= -1 then x
						else y 
						where
						x=checkZero state actions 
						z=map (getQ state) actions
						yval=foldl greater -inf z
						y=findAction state actions yval

