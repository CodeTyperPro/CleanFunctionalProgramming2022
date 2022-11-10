implementation module Stack

import StdEnv

:: Stack a :== [a]

newStack :: Stack a
newStack = []

empty  :: (Stack a) -> Bool
empty  [] = True
empty  x  = False

push :: a (Stack a) -> Stack a
push e s = [e : s]

pushes :: [a] (Stack a) -> Stack a 
pushes list s = (reverse list) ++ s

pop	:: (Stack a) -> Stack a
pop [e:s] = s

popn :: Int (Stack a) -> Stack a 
popn n s = drop n s

top	:: (Stack a) -> a
top [e:s] = e

topn     :: Int (Stack a) -> [a]
topn n s = take n s

elements :: (Stack a) -> [a]
elements x = x

count :: (Stack a) -> Int
count x = length x

//	You can use this Start-function to test your implementations:
Start				= ( "s0 = newStack = ",        s0,'\n'
					  , "s1 = push 1 s0 = ",       s1,'\n'
					  , "s2 = push 2 s1 = ",       s2,'\n'
					  , "s3 = pop s2 = ",          s3,'\n'
					  , "s5 = top s3 = ",          s5,'\n'
					  , "test = empty s1 = ",     test,'\n'
					  , "count s1 = ",     n,'\n'
					  , "pushes [1,2] s1 = ",     s6,'\n'
					  , "popn 2 s6 = ",           s7,'\n'
					  , "count s7 = ",            k,'\n')
where
	s0				= newStack
	s1				= push   1      s0
	s2				= push   2      s1
	s3				= pop           s2
	s5				= top           s3
	test            = empty         s1
	n               = count s1
	s6 				= pushes [1,2] s1
	s7 				= popn 2 s6
	k               = count s7