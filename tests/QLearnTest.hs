-- runhaskell   -i/home/arnab/Courses/Functional\ Programming/RLearnHaskell/src QLearnTest.hs
module Main where
import QLearn
--main = putStrLn "Hello World"

actions=[1,2,3]

initializeTest::Table->[Int]->Table
initializeTest q []=q
initializeTest q (state:restStates)= let{
	newQ=initializeState state actions q;
	resultQ=initializeTest newQ restStates;
	}in resultQ

	