module ex5_todo

import StdEnv


// 1. generate a list with 10 times of 13 : [13,13,13,13,13,13,13,13,13,13]
//l1 :: [Int]
//l1 = [

//Start = l1


// 2. generate the following list [(1,1),(1,2),(2,1),(2,2)]
//l2 :: [(Int, Int)]
//l2 = [(x,y) 

//Start = l2


// 3. generate the following list [(1,3),(1,2),(1,1),(2,3),(2,2),(2,1),(3,3),(3,2),(3,1)]
//l3 :: [(Int, Int)]
//l3 = [(x,y) 

//Start = l3


// 4. generate the list [(1,5),(2,6),(3,7),(4,8),(5,9),(6,10)]
//l4 :: [(Int, Int)]
//l4 = [(x,y) 

//Start = l4


// 5. generate the list [1,2,2,3,3,3,4,4,4,4,...,10,..,10]
//l5 :: [Int]
//l5 = [ y \\ 
//l5 = [ snd (x,y) 

//Start = l5


// 6. generate the list [[1],[2,2],[3,3,3],[4,4,4,4],...,[10,..,10]]
//l6 :: [[Int]]
//l6 = [ 

//Start = l6
  
  
// 7. generate 6 pythagoras numbers : [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20)]
//l7 :: [(Int, Int, Int)]
//l7 =  take 6 [(a,b,c) \\ c <- [1..], b<-[1..c], a<-[1..b] | a*a + b*b == c*c]
//l7 = [(a,b,c) \\ c <- [1..100], b<-[1..c], a<-[1..b] | a*a + b*b == c*c]
//l7 =  take 6 [(a,b,c) \\ a<-[1..100], b<-[1..100],c <- [1..100]  | a*a + b*b == c*c && a<b && b<c]

//Start = l7
