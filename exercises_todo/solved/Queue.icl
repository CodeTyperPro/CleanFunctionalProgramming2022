implementation module Queue

import StdEnv 

:: Queue a :== [a]


newQueue ::     (Queue a)                 // empty queue
newQueue = []

empty    ::     (Queue a) -> Bool
empty [] = True
empty _ = False

push     ::  a  (Queue a) -> Queue a   
push x q = q ++ [x]

pushes   :: [a] (Queue a) -> Queue a
pushes x q = q ++ x

pop      ::     (Queue a) -> Queue a   
pop q = tl q

popn     :: Int (Queue a) -> Queue a   
popn n q = drop n q

top      ::     (Queue a) -> a         
top q = hd q

topn     :: Int (Queue a) -> [a]        
topn n q = take n q

elements ::     (Queue a) -> [a]   
elements q = q

count    ::     (Queue a) -> Int 
count q = length q

//Start = newQueue
q1 = push 1 newQueue 
q2 = push 2 q1 
q3 = push 3 q2
q4 = push 4 q3
//Start = q4 // [1,2,3,4]
//Start = top q2 // 1
//Start = count q2 // 2
//Start = pop q4 // [2,3,4]
//Start = pushes [6,7,8] q4 // [1,2,3,4,6,7,8]
//Start = popn 3 q4 // [4]
//Start = elements q4 // [1,2,3,4]
Start = count q4 // 4