module ex5_todo

import StdEnv


// 1. generate a list with 10 times of 13 : [13,13,13,13,13,13,13,13,13,13]
l1 :: [Int]
l1 = [13 \\ x <- [1..10]]

//Start = l1


// 2. generate the following list [(1,1),(1,2),(2,1),(2,2)]
l2 :: [(Int, Int)]
l2 = [(x,y) \\ x <- [1,2] , y <- [1,2]]

//Start = l2


// 3. generate the following list [(1,3),(1,2),(1,1),(2,3),(2,2),(2,1),(3,3),(3,2),(3,1)]
l3 :: [(Int, Int)]
l3 = [(x,y) \\ x <- [1..3] , y <- [3,2,1]]

//Start = l3


// 4. generate the list [(1,5),(2,6),(3,7),(4,8),(5,9),(6,10)]
l4 :: [(Int, Int)]
l4 = [(x,y) \\ x <- [1..6] & y <- [5..]]

//Start = l4


// 5. generate the list [1,2,2,3,3,3,4,4,4,4,...,10,..,10]
l5 :: [Int]
l5 = [ y \\  y <-[1..10], x <- [1..y]]
l51 = [ snd (x,y) \\  y <-[1..10], x <- [1..y]]
l52 = flatten [ repeatn y y \\  y <-[1..10]]
//Start = l52


// 6. generate the list [[1],[2,2],[3,3,3],[4,4,4,4],...,[10,..,10]]
l6 :: [[Int]]
l6 = [ [y \\ x <- [1..y] ] \\ y <-[1..10]]

l62 = [ repeatn y y \\  y <-[1..10]]

//Start = l6
  
  
// 7. generate 6 pythagoras numbers : [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20)]
l7 :: [(Int, Int, Int)]
l7 =  take 100 [(a,b,c) \\ c <- [1..], b<-[1..c], a<-[1..b] | a*a + b*b == c*c]
//l7 = [(a,b,c) \\ c <- [1..100], b<-[1..c], a<-[1..b] | a*a + b*b == c*c]
//l7 =  take 6 [(a,b,c) \\ a<-[1..100], b<-[1..100],c <- [1..100]  | a*a + b*b == c*c && a<b && b<c]

Start = l7
