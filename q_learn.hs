import Data.Map (Map)
import qualified Data.Map as Map

-- fe for feedback 
-- st for state
-- ac for action
-- qval for value of q

inf=1e9

q= Map.empty

getQ::st->ac->qval
getQ state action=q Map.! (state,action)

greater::(Ord a)=>a->a->a
greater x y = if x>y then x else y

getMaxActionVal::(Ord qval)=> st-> [ac]->qval
getMaxActionVal newState actions=let x=map (getQ newState) actions
									in foldl greater -inf x  

giveFeedback::fe->st->[ac]
giveFeedback feedback newState actions=let maxim=0
										maxim=getMaxActionVal actions
										