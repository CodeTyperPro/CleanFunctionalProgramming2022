module mt4

import StdEnv



/* 1. Characters changed
Given a list of characters, write a function that replaces the character to corresponding integer.
For instance character 'a' and 'A' should be replaced with integer '1'.
*/

toIntList :: [Char] -> [Int]
toIntList list = [f x \\ x <- list ]
where f c
	  | isUpper c = toInt c - 64
	  | isLower c = toInt c - 96


//Start = toIntList ['a','A'] // [1,1]
//Start = toIntList ['a','z','b','B'] // [1,26,2,2]
//Start = toIntList [] // []



/* 2. Replace middle
Given a list of lists of Integers and an Integer, write a function that replaces the middle element
with the Integer in every sublist. 
*/

insmid :: [Int] Int -> [Int]
insmid list n = take (l/2) list ++ [n] ++ drop (l/2+1) list
where l = length list

insRep :: [[Int]] Int -> [[Int]]
insRep lists n = [insmid x n \\ x <- lists]

//Start = insRep [[1,2,3],[1..4]] 10 // [[1,10,3],[1,2,10,4]]
//Start = insRep [[1..10], [10,9..(-1)],[0,0,0]] 52 // [[1,2,3,4,5,52,7,8,9,10],[10,9,8,7,6,5,52,3,2,1,0,-1],[0,52,0]]
//Start = insRep [[], [1]] 4 // [[4],[4]]



/* 3. Powers
Given a list of Integers and an Integer, write a function which returns a list 
which only contains the powers of the integer.
*/

isPower :: Int Int -> Bool
isPower 1 _ = True
isPower x n
| x rem n == 0 = isPower (x/n) n
= False

powersList :: [Int] Int -> [Int]
powersList list n = [x \\ x <- list | isPower x n]

//Start = powersList [2,4,8,16,32,33,55] 2 // [2,4,8,16,32]
//Start = powersList [] 3 // []
//Start = powersList [1..10] 3 // [1,3,9]
//Start = powersList [-1,-2,4,8] 4 // [4]



/* 4. Smallest subArray
Given a list of integer numbers and an integer, find the smallest consecutive
subarray the sum of which is greater than the given integer.
Example :
        Integer =  97
        Smallest subarray in  [ 1, 5, 20, 70, 8] whose sum is greater than 97 is  [20, 70, 8]
        the output should be [20, 70, 8]  
*/

f :: [Int] Int Int-> [Int]
f list len n
| isEmpty (g list len 0 n) = f list (len+1) n
= g list len 0 n

g :: [Int] Int Int Int -> [Int]
g list len y n
| length list < y + len = []
| sum list1 > n = list1
= g list len (y+1) n
where 
	list2 = drop y list
	list1 = take len list2	
	
lengthOfSmallestSubArray ::  [Int] Int -> [Int]
lengthOfSmallestSubArray list n = f list 2 n

//Start = lengthOfSmallestSubArray [1, 5, 20, 70, 8] 97 // [20, 70, 8]
//Start = lengthOfSmallestSubArray [1..100] 56 // [28,29]
//Start = lengthOfSmallestSubArray [1..23] 275 // [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]



/* 5. Matrix
	Given a 3x3 matrix of '0's and '1's return if a line is formed from the given '1's.
	E.g: for the following matrix the output should be true.
	[ [1,0,0],
	  [0,1,0],
	  [0,0,1] ]
*/

sums :: [[Int]] -> [Int]
sums lists = [x,y,z]
where x = sum [a !! 0 \\ a <- lists]
	  y = sum [a !! 1 \\ a <- lists]
	  z = sum [a !! 2 \\ a <- lists]

matrix :: [[Int]] -> Bool
matrix lists = sums lists == [1,1,1]

//Start = matrix [[0,0,1], [0,1,0], [1,0,0]] 	// True
//Start = matrix [[1,1,1], [0,0,0] , [0,0,0]]  	// True
//Start = matrix [[1,0,0], [0,1,0] , [0,0,1]] 	// True
//Start = matrix [[1,0,0], [0,0,1], [1,1,0]] 	// False



/* 6. Closest avg
Given a list of lists of real numbers, for every sublist find the item in the sublist which is closest 
to the average of the sublist. It can be assumed there are no empty sublists.
e.g [[1.3, 5.2, 7.7, -2.3, 23.45] , [3.0,8.4] ] ->  avg of [1.3, 5.2, 7.7, -2.3, 23.45]  is 7.07 
so the closest value from the list is  7.7
similarly  avg of [3.0,8.4] is 5.7  so the closest value from the list is 3.0

*/

minInd :: [Real] -> Int
minInd list = hd [i \\ i<-[0..(length list)-1] | minList list == list!!i]

//Start = minInd [1.3, 5.2, 7.7, -2.3, 23.45]

diffs :: [Real] -> [Real]
diffs list = [abs (aver list - x) \\ x <- list]
where aver y = sum y / toReal (length y) 

//Start = diffs [1.3, 5.2, 7.7, -2.3, 23.45]

closestToAvg :: [[Real]] -> [Real]
closestToAvg lists = [x !! minInd (diffs x) \\ x <- lists]

//Start = closestToAvg [[1.3, 5.2, 7.7, -2.3, 23.45] , [3.0,8.4] ] //  [7.7,3] 
//Start = closestToAvg [[2.4 ,4.5 ,6.7 ,6.6 ,7.7] , [5.6 , 6.8 ,4.8 , 4.1] , [5.5,5.1] , [5.0] , [7.8] ] // [6.6,5.6,5.5,5,7.8]
//Start = closestToAvg [[1.3]] // [1.3]



/* 7. Matrix Transpose
Given a list of lists representing a matrix (a sublist represents a row of a matrix),
write a function that returns the transpose of the matrix. 
eg. [[1,2,3,4],[5,6,7,8],[9,10,11,12]] -> [[1,5,9],[2,6,10],[3,7,11],[4,8,12]] 
*/

makeTranspose :: [[Int]] -> [[Int]]
makeTranspose lists = [[l !! x \\ l <- lists] \\ x <- [0..length (hd lists)-1]]

//Start = makeTranspose []
//Start = makeTranspose [[1,2,3,4],[5,6,7,8],[9,10,11,12]] // [[1,5,9],[2,6,10],[3,7,11],[4,8,12]]
//Start = makeTranspose [[1,2,3,4,5,6,7,9], [-1,-2,-3,-4,-5,-6,-7,-8,-9]] // [[1,-1],[2,-2],[3,-3],[4,-4],[5,-5],[6,-6],[7,-7],[9,-8]]



/* 8. Tuple property
Given a list of lists of triple tuples of type integer, calculate the sum of (odd/even) numbers in every tuple in the sublists 
depending on the index of the sublist in the outer list whether its even or odd, and then return a list  
of boolean values describing whether this sum  (even/odd)  has the same property (even/odd) as the index of its sublist.

e.g : [[(1,2,3)] , [(1,4,5)]] = [True,False] the sum of the even numbers in the first sublist is 2,
and since the sum is even and the position of this sublist is even (0) we return [True]
Similarly for the second sublist [(1,4,5)] the sum of the odd numbers in it is 6, and its index in the outer list (1) is odd,
we return [False]. The final result is [True, False].
*/

toList :: (Int, Int, Int) -> [Int]
toList (a,b,c) = [a,b,c]

fx :: [Int] (Int->Bool) -> Int
fx [] p = 0
fx [x:xs] p
| p x = x + fx xs p
= fx xs p

//Start = fx (toList (1,2,4)) isEven

sumtup :: [(Int, Int, Int)] (Int->Bool) -> Int
sumtup tups prop = sum [(fx (toList t) prop) \\ t <- tups]

tupleProperty :: [[(Int,Int,Int)]] -> [Bool]
tupleProperty lists 
| isEven len = flatten [[isEven (sumtup (lists !!i) isEven), isOdd (sumtup (lists !!(i+1)) isOdd)]  \\  i<-[0,2..len-1]]
= flatten [[isEven (sumtup (lists !!i) isEven), isOdd (sumtup (lists !!(i+1)) isOdd)]  \\  i<-[0,2..len-2]] ++ [isEven (sumtup (last lists) isEven)]
where len = length lists

//Start = tupleProperty [[(1,1,3), (1,1,1)] , [(1,4,5)]] // [True,False] 
//Start = tupleProperty [[(1,1,3), (1,1,1)] , [(1,4,5) , (1,4,5)]  , [(1,4,5) , (1,4,5)]  , [(0,4,3) , (1,3,5)]] // [True,False,True,False]
//Start = tupleProperty [] // [] 
//Start = tupleProperty [[(1,1,3), (1,1,1)] , [(1,4,5) , (1,4,5)]  , [(1,4,5) , (1,4,5)]]
//Start = tupleProperty [[(1,2,3)]]



/* 9. Sine function
Taylor's series for approximating the sin value is as follows:
sin x = x - (x^3/3!) + (x^5/5!) - (x^7/7!) ... n times 
The bigger the n, the closer the approximation of the sin value. 
In our case, the n will be a static number with the value of 50.
Write a function that implements the Taylor's series to find the sin value of a given number, where n=50.
*/

fr :: Real -> Real
fr 1.0 = 1.0
fr r = r * fr (r - 1.0) 

sin :: Real -> Real 
sin 0.0 = 0.0
sin x = sum [(x^i)/(fr i) \\ i <- [1.0, 5.0..50.0]] - sum [(x^i)/(fr i) \\ i <- [3.0, 7.0..50.0]]

//Start = sin 0 // 0
//Start = sin 0.523599 // 0.500000194337561
//Start = sin 1.0472 // 0.866026628183543
//Start = sin 1.5708 // 0.999999999993254



/* 10. Duplicates
Given a list of lists, make a function that finds the duplicate element in each sublist,
and replaces all of the elements of the sublist before it with the duplicate element. The last sublist's elements should
be replaced with the first sublist's duplicate element. If one of the sublists is empty or has no duplicates,
the list before it should be replaced with an empty list
[[120,440,112,42,42,0], [110, 42, 402, 110], [12,34,5,2,6,2],[34,34]] ->
[[110,110,110,110,110,110], [2, 2, 2, 2], [34,34,34,34,34,34],[42,42]]
*/

dup :: [Int] -> Int
dup [] = abort "no duplicate"
dup [x:xs]
| isMember x xs = x
= dup xs

rep :: [Int] [Int] -> [Int]
rep x y
| removeDup y == y = []
= repeatn (length x) (dup y)

mapDuplicate :: [[Int]] -> [[Int]]
mapDuplicate lists = [rep (lists!!(i-1)) (lists!!i) \\ i <- [1..length lists-1]] ++ [rep (last lists) (lists !! 0)]

//Start = mapDuplicate [[120,440,112,42,42,0], [110, 42, 402, 110], [12,34,5,2,6,2],[34,34]] // [[110,110,110,110,110,110],[2,2,2,2],[34,34,34,34,34,34],[42,42]]
//Start = mapDuplicate [[1,2,3,4,5,6,7,7], [1,2], [1,2,3,4,5,5], [6]] // [[],[5,5],[],[7]]