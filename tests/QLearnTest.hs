-- runhaskell   -i/home/arnab/Courses/Functional\ Programming/RLearnHaskell/src QLearnTest.hs
module Main where
import QLearn
import Data.Map (Map)
import qualified Data.Map as Map

--main = putStrLn "Hello World"

actions=[1,2,3]

initializeTest::Table->[Int]->Table
initializeTest q []=q
initializeTest q (state:restStates)= let{
	newQ=initializeState state actions q;
	resultQ=initializeTest newQ restStates;
	}in resultQ


driveQLearn::Table->[(Int,Double)]->Table
driveQLearn q ((state,feedback):rest)=


main= do
		let q=Map.empty
		let initializedQ=initializeTest q [1,2,3,4,5,6]
		let val=Map.showTree initializedQ	
		return initializedQ
